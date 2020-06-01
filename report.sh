#!/bin/sh
set -v
./stats_report csv
./stats_report flat
./stats_report html
