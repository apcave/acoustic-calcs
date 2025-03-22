import json
import math
import argparse


def compare_json(json1, json2, tolerance=1e-6):
    """Recursively compare two JSON objects with a numerical tolerance."""
    if isinstance(json1, dict) and isinstance(json2, dict):
        if json1.keys() != json2.keys():
            return False
        return all(compare_json(json1[key], json2[key], tolerance)
                   for key in json1)
    elif isinstance(json1, list) and isinstance(json2, list):
        if len(json1) != len(json2):
            return False
        return all(compare_json(item1, item2, tolerance)
                   for item1, item2 in zip(json1, json2))
    elif isinstance(json1, float) and isinstance(json2, float):
        if math.isclose(json1, json2, rel_tol=tolerance):
            return True
        else:
            print(f"Floats {json1} and {json2} are not close.")
            return False
    else:
        if json1 == json2:
            return True
        else:
            print(f"Values {json1} and {json2} are not equal.")
            return False


def main():
    parser = argparse.ArgumentParser(
        description='Compare two JSON files with numerical tolerance.')
    parser.add_argument('file1', type=str, help='First JSON file')
    parser.add_argument('file2', type=str, help='Second JSON file')
    parser.add_argument('--tolerance', type=float, default=1e-6,
                        help='Numerical tolerance for float comparisons')

    args = parser.parse_args()

    print(f"Comparing {args.file1} and {args.file2} with tolerance {args.tolerance}")

    # Load JSON content from files
    with open(args.file1, 'r') as f:
        json1 = json.load(f)
    with open(args.file2, 'r') as f:
        json2 = json.load(f)

    # Compare JSON data with a tolerance
    if compare_json(json1, json2, args.tolerance):
        print("The JSON objects are equal within the given tolerance.")
    else:
        print("The JSON objects are not equal within the given tolerance.")


if __name__ == "__main__":
    main()