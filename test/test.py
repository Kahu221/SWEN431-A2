import subprocess
import os

# List of test IDs
test_ids = [
    "001", "002", "003", "004", "005", "010", "011", "101", "102", "103", "104", "110", "111",
    "120", "130", "131", "140", "150", "151", "152", "153", "200", "210", "220", "222", "225",
    "230", "232", "234", "240", "242", "250", "252", "260", "999"
]

# Paths
project_root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
ws_path = os.path.join(project_root, "ws.hs")
inputs_path = os.path.join(project_root, "inputs")
expected_path = os.path.join(project_root, "expected")
results_log = os.path.join(project_root, "test", "test_results.txt")

# Normalize line endings for comparison
def normalize(s):
    return s.replace('\r\n', '\n').replace('\r', '\n').strip()

# Run a single test
def run_test(test_id):
    input_file = os.path.join(inputs_path, f"input-{test_id}.txt")
    expected_file = os.path.join(expected_path, f"expected-{test_id}.txt")
    output_file = os.path.join(project_root, f"output-{test_id}.txt")

    try:
        subprocess.run(["runghc", ws_path, input_file], check=True)

        if not os.path.exists(output_file):
            return f"❌ Test {test_id}: Output file not found."

        with open(output_file, "r", encoding="utf-8") as fout, open(expected_file, "r", encoding="utf-8") as fexp:
            output = normalize(fout.read())
            expected = normalize(fexp.read())

        if output == expected:
            os.remove(output_file)
            return f"✅ Test {test_id}: Passed."
        else:
            return f"❌ Test {test_id}: Failed (output kept)."

    except subprocess.CalledProcessError:
        return f"❌ Test {test_id}: Runtime error."
    except Exception as e:
        return f"❌ Test {test_id}: {str(e)}"

# Run all tests and write results
results = [run_test(tid) for tid in test_ids]

with open(results_log, "w", encoding="utf-8") as log:
    for line in results:
        print(line)
        log.write(line + "\n")
