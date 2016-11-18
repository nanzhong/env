import json
import subprocess

def secure_string_for(account, service, value):
    return json.loads(subprocess.check_output(["security",
                                               "find-generic-password",
                                               "-a", account,
                                               "-s", service,
                                               "-w"]).strip())[value]
