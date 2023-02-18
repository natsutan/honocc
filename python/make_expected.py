import os
import glob
import subprocess
import sys

CSRC_ROOT_DIR = "test/csrc"
EXPECTED_ROOT_DIR = "test/expected"


def create_expected(cat, csrc):
    test_name = csrc.split('.')[0]
    exe_path = os.path.join(EXPECTED_ROOT_DIR, cat, test_name)

    # compile
    cmd = ['gcc',
           '-Wno-implicit-function-declaration',
           os.path.join(CSRC_ROOT_DIR, 'common', 'hlib.c'),
           os.path.join(CSRC_ROOT_DIR, cat, csrc),
           '-o',
           exe_path
           ]

    result = subprocess.run(cmd)
    if result.returncode != 0:
        print("Error:", cmd)
        sys.exit(1)

    # execute
    cmd = exe_path + " > " + exe_path + ".txt"
    result = subprocess.run(cmd, shell=True)
    if result.returncode != 0:
        print("Error:", cmd)
        sys.exit(1)

    print(cmd)


def create_category_expected(cat):
    expected_dir = os.path.join(EXPECTED_ROOT_DIR, cat)
    os.makedirs(expected_dir, exist_ok=True)

    c_sources = glob.glob(os.path.join(CSRC_ROOT_DIR, cat) + "/*.c")
    for csrc in c_sources:
        src_file = csrc.split('/')[-1]
        create_expected(cat, src_file)


def main():
    pwd = os.getcwd()
    print("current dir = ", pwd)

    categories = os.listdir(path=CSRC_ROOT_DIR)
    categories.remove('common')
    print(categories)

    for cat in categories:
        create_category_expected(cat)


if __name__ == "__main__":
    main()