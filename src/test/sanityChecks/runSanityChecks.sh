#!/usr/bin/env bash

function runTestDifferentDelimitedFiles {
    local l_quickJarPath="${1}"
    local l_sanityChecksDirectory="${2}"
    java -jar "${l_quickJarPath}" -u -d "${l_sanityChecksDirectory}/filesDescriptions.xml" -i delimited "${l_sanityChecksDirectory}/delimited/different/ordersTableDump-01-12-2017.csv" "${l_sanityChecksDirectory}/delimited/different/ordersTableDump-01-01-2018.csv"
}

function runTestIdenticalDelimitedFiles {
    local l_quickJarPath="${1}"
    local l_sanityChecksDirectory="${2}"
    java -jar "${l_quickJarPath}" -u -d "${l_sanityChecksDirectory}/filesDescriptions.xml" -i delimited "${l_sanityChecksDirectory}/delimited/identical/ordersTableDump-01-12-2017.csv" "${l_sanityChecksDirectory}/delimited/identical/ordersTableDump-01-01-2018.csv"
}

function runTestDelimitedFilesWithChecks {
    local l_quickJarPath="${1}"
    local l_sanityChecksDirectory="${2}"
    java -jar "${l_quickJarPath}" -u -d "${l_sanityChecksDirectory}/filesDescriptions.xml" -i delimited "${l_sanityChecksDirectory}/delimited/checks/ordersTableDump-01-12-2017.csv" "${l_sanityChecksDirectory}/delimited/checks/ordersTableDump-01-01-2018.csv"
}

function runTestDifferentFixedLengthFiles {
    local l_quickJarPath="${1}"
    local l_sanityChecksDirectory="${2}"
    java -jar "${l_quickJarPath}" -u -d "${l_sanityChecksDirectory}/filesDescriptions.xml" -i fixedlength "${l_sanityChecksDirectory}/fixedlength/different/ordersTableDump-01-12-2017.csv" "${l_sanityChecksDirectory}/fixedlength/different/ordersTableDump-01-01-2018.csv"
}

function runTestIdenticalFixedLengthFiles {
    local l_quickJarPath="${1}"
    local l_sanityChecksDirectory="${2}"
    java -jar "${l_quickJarPath}" -u -d "${l_sanityChecksDirectory}/filesDescriptions.xml" -i fixedlength "${l_sanityChecksDirectory}/fixedlength/identical/ordersTableDump-01-12-2017.csv" "${l_sanityChecksDirectory}/fixedlength/identical/ordersTableDump-01-01-2018.csv"
}

function runTestFixedLengthFileWithChecks {
    local l_quickJarPath="${1}"
    local l_sanityChecksDirectory="${2}"
    java -jar "${l_quickJarPath}" -u -d "${l_sanityChecksDirectory}/filesDescriptions.xml" -i fixedlength "${l_sanityChecksDirectory}/fixedlength/checks/ordersTableDump-01-12-2017.csv" "${l_sanityChecksDirectory}/fixedlength/checks/ordersTableDump-01-01-2018.csv"
}

function runSanityCheckTests {
    local l_sanityCheckDirectory="${1}"
    local l_targetDirectory="${l_sanityCheckDirectory}/../../../target"
    local l_quickJarPath=''
    for l_quickJarPath in $( find ${l_targetDirectory} -type f | grep -E 'quick.jar$'); do
        echo "runTestDifferentDelimitedFiles"
        runTestDifferentDelimitedFiles "${l_quickJarPath}" "${l_sanityCheckDirectory}"

        echo "runTestIdenticalDelimitedFiles"
        runTestIdenticalDelimitedFiles "${l_quickJarPath}" "${l_sanityCheckDirectory}"

        echo "runTestDelimitedFilesWithChecks"
        runTestDelimitedFilesWithChecks "${l_quickJarPath}" "${l_sanityCheckDirectory}"

        echo "runTestDifferentFixedLengthFiles"
        runTestDifferentFixedLengthFiles "${l_quickJarPath}" "${l_sanityCheckDirectory}"

        echo "runTestIdenticalFixedLengthFiles"
        runTestIdenticalFixedLengthFiles "${l_quickJarPath}" "${l_sanityCheckDirectory}"

        echo "runTestFixedLengthFileWithChecks"
        runTestFixedLengthFileWithChecks "${l_quickJarPath}" "${l_sanityCheckDirectory}"
    done
}

function runTestsAndPrintDiff {
    local l_userCommand="${1}"
    local l_sanityCheckDirectory="$( dirname ${l_userCommand} )"
    runSanityCheckTests "${l_sanityCheckDirectory}" | sdiff -lbs "${l_sanityCheckDirectory}/expectedTestOutput" -
}

runTestsAndPrintDiff "${0}"
