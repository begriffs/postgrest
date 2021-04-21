"Update the expected schema dump."
import json
import os
import pathlib
import shutil
import subprocess
import yaml


BASEDIR = pathlib.Path(os.path.realpath(__file__)).parent


class ExtraNewlinesDumper(yaml.SafeDumper):
    "Dumper that inserts an extra newline after each top-level item."

    def write_line_break(self, data=None):
        super().write_line_break(data)

        if len(self.indents) == 1:
            super().write_line_break()


def update_schema_dump(outputdir):
    shutil.rmtree(outputdir, ignore_errors=True)
    outputdir.mkdir(parents=True)
    command = ["postgrest", "--dump-schema"]
    result = subprocess.run(command, capture_output=True, check=True)
    schema = json.loads(result.stdout)

    for key, data in schema.items():
        outputfile = outputdir / f"{key}.yaml"
        outputfile.write_text(yaml.dump(data, Dumper=ExtraNewlinesDumper))


if __name__ == "__main__":
    update_schema_dump(BASEDIR / "expected")
