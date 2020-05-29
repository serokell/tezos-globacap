#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2020 Globacap
# SPDX-License-Identifier: MPL-2.0

setup () {
  bin_dir="tmp"
  globacap="$bin_dir/globacap"
  dummyAddress="tz1akcPmG1Kyz2jXpS4RvVJ8uWr7tsiT9i6A"
  dummyContractAddress="KT1KNtPCj1UUqXwxyQdCgXckqmU324bmsJcV"
}

@test "Invoking with --help" {
  "$globacap" --help
}

@test "Invoking with --version" {
  "$globacap" --version
}

@test "List available contracts" {
  "$globacap" list
}

@test "Print Holdings" {
  "$globacap" print -n Holdings
}

@test "Print initial Holdings storage" {
  "$globacap" storage-Holdings --owner "$dummyAddress" --admin "$dummyAddress" \
    --safelist-address "$dummyContractAddress" --token-name "TokenKek" \
    --token-symbol "Kk" --token-id "KEK"
}

@test "Document Holdings" {
  "$globacap" document -n Holdings
}
