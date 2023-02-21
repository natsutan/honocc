import os
import glob
import sys
import subprocess
import difflib

CSRC_ROOT_DIR = "test/csrc"
EXPECTED_ROOT_DIR = "test/expected"
BUILD_DIR = 'honocc'
COMPILER_PATH = 'honocc/bin/Debug/net6.0/honocc'

total_test_cnt = 0
x64_passed = 0
riscv_passed = 0


def compile(target, cat, src):

    target_src = os.path.join(CSRC_ROOT_DIR, cat, src)
    # honocc/bin/Debug/net6.0/honocc --output-dir test/x64/00_base test/csrc/00_base/first.c
    cmd = COMPILER_PATH + " --output-dir-x64 test/x64/" + cat + " --output-dir-riscv test/riscv/" + cat + " " + target_src + " > /dev/null "
    result = subprocess.run(cmd, shell=True)
    if result.returncode != 0:
        print("Error:", cmd)
        return False

    return True

def link(target, cat, src):
    hlib_path = os.path.join(CSRC_ROOT_DIR, "common", "hlib.c")
    base_name = os.path.basename(src).split('.')[0]
    asm_file = base_name + ".s"

    exe_path = os.path.join('test', target, cat, base_name)
    asm_path = os.path.join('test', target, cat, asm_file)

    if target == 'x64':
        cmd = "gcc " + asm_path + " " + hlib_path + " -o " + exe_path
    else:
        cmd = "riscv64-unknown-elf-gcc " + asm_path + " " + hlib_path + " -o " + exe_path + ".riscv"

    result = subprocess.run(cmd, shell=True)
    if result.returncode != 0:
        print("Error:", cmd)
        return False

    return True

def run(target, cat, src):
    base_name = os.path.basename(src).split('.')[0]
    exe_path = os.path.join('test', target, cat, base_name)
    log_file = exe_path + ".txt"

    if target == 'x64':
        cmd = exe_path + " > " + log_file
    else:
        cmd = "spike pk " + exe_path + ".riscv > " + log_file

    result = subprocess.run(cmd, shell=True)
    # 今 mainの戻り地が適当なので、変な値が帰ってくる。
    # if result.returncode != 0:
    #     print("Error:", cmd)
    #     return False

    return True

def compare(target, cat, src):
    base_name = os.path.basename(src).split('.')[0]
    expeted_path = os.path.join(EXPECTED_ROOT_DIR, cat, base_name + ".txt")
    result_path = os.path.join('test', target, cat, base_name + ".txt")

    file1 = open(expeted_path)
    file2 = open(result_path)
    diff = difflib.Differ()

    first_line = 0
    if target == 'riscv':
        first_line = 1

    output_diff = diff.compare(file1.readlines(), file2.readlines()[first_line:])

    same = True
    for data in output_diff:
        data = data.rstrip('\n')
        if data[0] == '-':
            print("compare failed: expected:", data[2:],  end="")
        if data[0] == '+':
            print("  result", data[2:])
            same = False
            break
    file1.close()
    file2.close()

    return same

def run_test_single(target, cat, src):
    # compile
    if not compile(target, cat, src):
        return False

    # link
    if not link(target, cat, src):
        return False

    # run
    if not run(target, cat, src):
        return False

    # compare
    if not compare(target, cat, src):
        return False


    return True


def record_test(target, result, cat, csrc):
    global total_test_cnt, x64_passed, riscv_passed
    total_test_cnt += 1
    if result:
        print("PASS:", cat, ":", csrc)
        if target == 'x64':
            x64_passed += 1
        elif target == 'riscv':
            riscv_passed += 1
    else:
        print("ERROR:", cat, ":", csrc)


def run_category_test(target, cat):
    test_dir = os.path.join('test', target, cat)
    os.makedirs(test_dir, exist_ok=True)
    output_dir = os.path.join('test', target, cat)
    os.makedirs(output_dir, exist_ok=True)

    c_sources = glob.glob(os.path.join(CSRC_ROOT_DIR, cat) + "/*.c")
    for csrc in c_sources:
        src_file = csrc.split('/')[-1]
        result = run_test_single(target, cat, src_file)
        record_test(target, result, cat, csrc)


def run_test_all(target):
    global total_test_cnt
    total_test_cnt = 0
    categories = os.listdir(path=CSRC_ROOT_DIR)
    categories.remove('common')
    print(categories)

    for cat in categories:
        run_category_test(target, cat)

def build_honocc():
    pwd_save = os.getcwd()
    os.chdir(BUILD_DIR)

    cmd = '/home/natu/.dotnet/dotnet build'
    result = subprocess.run(cmd, shell=True)
    if result.returncode != 0:
        print("Error:", cmd)
        sys.exit(1)

    os.chdir(pwd_save)

def report(target):
    if target == 'x64':
        passed = x64_passed
    elif target == 'riscv':
        passed = riscv_passed

    print("ALL test (", target, ") finished passed(", passed, "/", total_test_cnt, ")")

def main():
    pwd = os.getcwd()
    print("current dir = ", pwd)

    build_honocc()

    print("---- start test for x64")
    run_test_all('x64')
    report('x64')

    print("")
    print("---- start test for riscv")
    run_test_all('riscv')
    report('riscv')


if __name__ == "__main__":
    main()