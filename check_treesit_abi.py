#!/usr/bin/env python3

import ctypes
import sys
from pathlib import Path

# Define the C struct for TSLanguage based on the tree-sitter.h header.
# We only need the first member, 'version'.
class TSLanguage(ctypes.Structure):
    _fields_ = [("version", ctypes.c_uint32)]

def get_abi_version(lib_path, lang_name):
    """Loads a Tree-sitter library and reads its ABI version."""
    try:
        # 1. Dynamically load the .so library
        lib = ctypes.CDLL(lib_path)

        # 2. Find the entry point function
        func_name = f"tree_sitter_{lang_name}"
        lang_func = getattr(lib, func_name)

        # 3. Tell ctypes what the function returns: a pointer to our struct
        lang_func.restype = ctypes.POINTER(TSLanguage)

        # 4. Call the function to get the pointer
        ts_lang_ptr = lang_func()

        # 5. Read the 'version' member from the struct
        abi_version = ts_lang_ptr.contents.version
        return abi_version

    except Exception as e:
        return f"Error: {e}"

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python check_abi.py <path_to_so_file> <language_name>")
        print("Example: python check_abi.py libtree-sitter-python.so python")
        sys.exit(1)
    lib_path = Path(sys.argv[1])
    if not lib_path.exists():
        msg = f"{lib_path} not found"
        raise FileNotFoundError(msg)

    lang_name = sys.argv[2]
    version = get_abi_version(str(lib_path.resolve()), lang_name)
    print(f"ABI Version for '{lib_path}': {version}")
