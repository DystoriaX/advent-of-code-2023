#!/bin/python3
import argparse
from dotenv import load_dotenv
import os
import requests
import subprocess

def load_input(day: int):
    inpath = f"input/day{day:02d}/input.in"

    if os.path.exists(inpath):
        print("Input file exists")
        return

    print(f"Loading inputs for day {day}")
    url = f"https://adventofcode.com/2023/day/{day}/input"

    load_dotenv()
    cookie = os.getenv('COOKIE')

    if not cookie:
        print("Requires COOKIE environment variable")
        exit(1)

    os.makedirs(os.path.dirname(inpath), exist_ok=True)
    with open(inpath, "w") as f:
        with requests.Session() as session:
            response = session.get(url, cookies={'session': cookie})
            f.write(response.text)

def run_prog(day: int, part: int):
    dirpath = os.path.relpath(f"src/day{day:02d}/")
    filename = f"part{part}"
    srcpath = os.path.join(dirpath, filename + ".hs")
    outpath = os.path.join(dirpath, filename + ".elf")

    subprocess.run(["ghc", srcpath, "-o", outpath])

    indir = os.path.relpath(f"input/day{day:02d}/")

    for file in os.listdir(indir):
        inpath = os.path.relpath(f"input/day{day:02d}/{file}")

        with open(inpath, "r") as f:
            print(f"{inpath}:")
            subprocess.run([outpath], stdin=f)

if __name__ == '__main__':
    parser = argparse.ArgumentParser("AOC 2023 Driver")
    parser.add_argument("-d", "--day", help="Day of AOC that you want to run", type=int, required=True)
    parser.add_argument("-l", "--load", help="Load input of a particular day", action="store_true")
    parser.add_argument("-p", "--part", help="Runs program for part {1,2}", choices=[1, 2], nargs='*', type=int)

    args = parser.parse_args()


    if args.load:
        load_input(args.day)

    if args.part:
        for p in args.part:
            run_prog(args.day, p)

