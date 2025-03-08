# ----------------------------------------------------------------------
#  Copyright (c) 2024 Egor Smirnov
#
#  Licensed under terms of the MIT license
#  See https://github.com/egorman44/Reed-Solomon/blob/main/LICENSE
#    for license terms
# ----------------------------------------------------------------------

import sys
import xml.etree.ElementTree as ET

def check_test_failures(xml_file):
    try:
        tree = ET.parse(xml_file)
        root = tree.getroot()

        # Find all <failure> elements
        failures = root.findall(".//failure")

        if failures:
            print(f"{len(failures)} test(s) failed.")
            sys.exit(1)  # Non-zero exit code indicates failure
        else:
            print("All tests passed.")
            sys.exit(0)  # Zero exit code indicates success

    except ET.ParseError as e:
        print(f"Error parsing XML: {e}")
        sys.exit(2)  # Exit code 2 for XML parsing issues
    except Exception as e:
        print(f"Unexpected error: {e}")
        sys.exit(3)  # Exit code 3 for other unexpected errors

if __name__ == "__main__":
    xml_file_path = "sim_build/results.xml"  # Change this to your actual file path
    check_test_failures(xml_file_path)

    

