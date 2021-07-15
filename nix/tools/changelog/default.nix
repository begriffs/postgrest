{ buildToolbox
, checkedShellScript
, git
, jq
, python3Packages
, yq
}:
let
  json =
    checkedShellScript
      {
        name = "postgrest-changelog-json";
        docs =
          ''
            Creates a changelog from `git log` in json format.
          '';
        args =
          [
            "ARG_POSITIONAL_SINGLE([from], [first commit-ish to take from git log])"
            "ARG_POSITIONAL_SINGLE([to], [last commit-ish to take from git log])"
          ];
        inRootDir = true;
      }
      ''
        export PATH="${jq}/bin:$PATH"

        function git_log_json () {
          #  is control character 26 SUB (Substitute)
          ${git}/bin/git log "$@" --reverse --pretty=format:"
            - author:
                name: %an
                email: %ae
              body: %B
              hash: %h
              subject: %s" \
            | sed -r "s/(')/\1\1/g" \
            | tr '' "'" \
            | ${yq}/bin/yq
        }

        function parse_author () {
          # shellcheck disable=SC2016
          ${jq}/bin/jq 'map(. + { author: (
                (.author.email | match("^(?:[0-9]+\\+)?(.+)@users\\.noreply\\.github\\.com").captures[0].string)
                  //
                $name2profile[.author.name]
                  //
                { name: .author.name }
              )})' --argfile name2profile ${./author-map.json}
        }

        function parse_references () {
          ${jq}/bin/jq 'map(. + { refs: (
                .body | match("#([0-9]+)"; "g").captures // [] | map(.string | tonumber)
              )})'
        }

        function parse_type () {
          ${jq}/bin/jq 'map(. + { type: (
                if (.body | test("BREAKING CHANGE"))
                then "breaking"
                else (.subject | match("^(fix|feat):").captures[0].string // "other")
                end
              )})'
        }

        function cleanup () {
          ${jq}/bin/jq 'map(
                del(.body)
                | if (.refs | length) == 0
                  then del(.refs)
                  else .
                  end
              )'
        }

        function to_map () {
          ${jq}/bin/jq 'map({ (.hash): del(.hash) }) | add'
        }
        
        git_log_json "$_arg_from".."$_arg_to" \
          | parse_author \
          | parse_references \
          | parse_type \
          | cleanup \
          | to_map
      '';

  render =
    checkedShellScript
      {
        name = "postgrest-changelog-render";
        docs = "Renders a changelog from YAML or JSON input stream.";
        args =
          [
            "ARG_POSITIONAL_SINGLE([template], [path to template file to render])"
          ];
      }
      ''
        export PATH="${jq}/bin:$PATH"

        function from_map () {
          ${yq}/bin/yq '{ current: ._current, previous: ._previous } +
              (.| to_entries
                | map_values(
                    if (.value | type) == "object"
                    then .value + { hash: .key }
                    else null
                    end)
                | map(select(. != null))
                | group_by(.type)
                | map({ (.[0].type): . })
                | add
              )'
        }

        from_yaml_map | (${python3Packages.chevron}/bin/chevron -d /dev/stdin "$_arg_template")
      '';

  changelog =
    checkedShellScript
      {
        name = "postgrest-changelog";
        docs =
          ''
            Updates the changelog for the current version.
            TODO: Optionally renders the changelog via --render <template>.
          '';
        args =
          [
            "ARG_POSITIONAL_SINGLE([version], [git version tag to generate changelog for (can be any commit-ish value, defaults to HEAD)], [HEAD])"
          ];
        inRootDir = true;
      }
      ''
        export PATH="${jq}/bin:$PATH"

        function current_tag () {
          tag=$(${git}/bin/git tag --list v* nightly --points-at "$1" | sort -r | head -n 1)
          echo "''${tag:-$1}"
        }

        function previous_tag () {
          ${git}/bin/git tag --list v* nightly --no-contains "$1" | sort -r | head -n 1
        }

        function merge_files () {
          ${yq}/bin/yq --slurp add "$@"
        }

        function add_version () {
          # shellcheck disable=SC2016
          ${jq}/bin/jq --arg current "$1" --arg previous "$2" \
             '{ _current: $current, _previous: $previous } + .'
        }

        function format_yaml () {
          ${yq}/bin/yq --yml-output --indentless-lists --width 1000 \
            | sed -r 's/^(\S)/\n\1/g'
        }

        current=$(current_tag "$_arg_version")
        previous=$(previous_tag "$current")

        mkdir -p changelog
        outfile="changelog/$current.yaml"
        touch "$outfile"

        tmpdir="$(mktemp -d)"
        trap 'rm -rf "$tmpdir"' SIGINT SIGTERM ERR
        ${json} "$previous" "$current" > "$tmpdir"/latest.yaml

        merge_files "$tmpdir"/latest.yaml "$outfile" \
          | add_version "$current" "$previous" \
          | format_yaml > "$tmpdir/$current.yaml"

        mv "$tmpdir/$current.yaml" "$outfile"
      '';

in
buildToolbox {
  name = "postgrest-changelog";
  tools = [ json render changelog ];
}
