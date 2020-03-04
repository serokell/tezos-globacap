<!-- SPDX-FileCopyrightText: 2020 TBD -->

<!-- SPDX-License-Identifier: LicenseRef-Proprietary -->

# Tezos Globacap

The purpose of this is to create a bridge between Asset Registry
on Hyperledger to Tezos. This means investors can have immediate liquidity
of the corresponding investment. This document explains the framework of the
smart contract.

To restrict and regulate share transfers `Safelist` contract is used.

## Safelist contract

Only admin can access all non-view functions.

Approximate storage structure:
```haskell
data Storage = Storage
  { owner :: Address
  , admins :: Set Address
  , whitelist :: Set Address
  , blacklist :: Set Address
  }
```

Required `Safelist` entrypoint:
* `addAdmin (address)`
  * Description: adds address to `admins`.
  * Constraints: `onlyOwner`.
* `removeAdmin (address)` onlyOwner.
  * Description: removes address from `admins`.
  * Constraints: `onlyOwner`.
* `isAdmin (view address bool)`
  * Description: returns `True` if the address is admin.
* `addToWhitelist (address)` onlyAdmin.
  * Description: adds address to `whitelist`.
  * Constraints: `onlyAdmin`.
* `removeFromWhitelist (address)` onlyAdmin.
  * Description: removes address from `whitelist`.
  * Constraints: `onlyAdmin`.
* `isWhitelisted (view address bool)`
  * Description: returns `True` if the address is whitelisted.
* `addToBlacklist (address)` onlyAdmin.
  * Description: adds address to `blacklist`.
  * Constraints: `onlyAdmin`.
* `removeFromBlacklist (address)` onlyAdmin.
  * Description: removes address from `blacklist`.
  * Constraints: `onlyAdmin`.
* `isBlacklisted (view address bool)`
  * Description: returns `True` if the address is blacklisted.

Additionaly, it would be nice to have the following entrypoint:
* `checkConstraints (((list address) :isWhitelisted), ((list address) :isNonBlacklisted))`.
  * Description: check for all addresses to be whitelisted in the former list, check
    for all addresses to be nonblacklisted in the latter list. Do nothing if everything
    is satisfied, fail otherwise.

This contract implements regulatory service for the **Holdings** conract.

## Holdings contract

Approximate storage structure:
```haskell
data Storage = Storage
  { name :: MText
  , symbol :: MText
  , id :: MText
  , safelistAddress :: Maybe (TAddress Safelist.Storage)
  , owner :: Address
  , admin :: Address
  , ledger :: BigMap Address (Map Address Natural, Natural)
  , paused :: Bool
  , transferable :: Bool
  , totalMinted :: Natural
  , totalBurned :: Natural
  }
```

Required `Holdings` entrypoints:
* `setName (string :newName)`
  * Description: updates `name` field.
  * Constraints: `onlyAdmin` `isNotPaused`.
* `setSymbol (string :newSymbol)`
  * Description: updates `symbol` field.
  * Constraints: `onlyAdmin` `isNotPaused`.
* `setSafelistAddress ((option address) :mbNewSafelistAddress)`
  * Description: updates `safelistAddress` field, which is used for safelist contract calls.
  * Constraints: `onlyOwner`.
* `transferAdminRights (address :newAdmin)`
  * Description: transfers `admin` rights to a new address. Transfer is not completed
    until the `newAdmin` calls `acceptAdminRights`.
  * Constraints: `onlyOwner` `isNotPaused`.
* `acceptAdminRights ()`.
  * Description: accepts `admin` rights.
  * Constraints: The sender has to be the `newAdmin`.
* `isAdmin (view address bool)`
  * Description: returns `True` if the address is admin.
* `transfer (address :from, address :to, nat :value)`
  * Description: transfer given amount of tokens from one address to another.
  * Constraints: `onlyAdmin`, `isNotPaused`, `isTransferable`.
  * Safelist constraints: `sender` is whitelisted, `sender` and `receiver` are nonblacklisted.
* `approve (address :spender, nat :value)`
  * Description: approve given amout of token to be spent by given address from `sender`.
  * Constraints: `isNotPaused`.
  * Safelist constraints: `spender` is whitelisted, `spender` and `sender` are nonblacklisted.
* `getAllowance (view (address :owner, address :spender) natural)`
  * Description: returns approval amount for two given addresses.
* `getBalance (view (address :owner) natural)`
  * Description: returns balance of given address.
* `getTotalSupply (view unit nat)`
  * Description: return total supply (`totalSupply = totalMinted - totalBurned`).
* `mint (address :investor, nat: value)`
  * Description: mint given amount of tokens to given address.
  * Constraints: `onlyAdmin`, `isNotPaused`.
  * Safelist constraints: `investor` is whitelisted and nonblacklisted.
* `burn (address :investor, nat :value)`
  * Description: burn amount given amount of tokens from given address.
  * Constraints: `onlyAdmin`, `isNotPaused`
* `burnAll ()`
  * Description: burn all tokens and remove all approvals.
  * Constraints: `onlyAdmin`, `isNotPaused`.
* `setPause (bool)`
  * Description: pause/unpause the contract. When paused no transactions should be possible on this contract.
  * Constraints: `onlyAdmin`.
* `setTransferable (bool: value)`
  * Description: update `transferable` flag. When this flag is set to false, transfers should not be possible.
  * Constraints: `onlyAdmin`.
