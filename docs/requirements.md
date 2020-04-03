<!-- SPDX-FileCopyrightText: 2020 TBD -->

<!-- SPDX-License-Identifier: LicenseRef-Proprietary -->

# Tezos Globacap

The purpose of this is to create a bridge between Asset Registry
on Hyperledger to Tezos. This means investors can have immediate liquidity
of the corresponding investment. This document explains the framework of the
smart contract.

There are two contracts:
1. `Safelist` contract is used to restrict and regulate share transfers.
2. `Holdings` contract is a token contract compliant with with FA1.2 interface.

We only implement the `Holdings` contract and describe all its entrypoints below.
`Safelist` contract should be implemented separately, here we only describe its entrypoints that are used in the `Holdings` contract.

## Safelist contract

Required `Safelist` entrypoints (used by `Holdings`):
* `assertTransfer (pair (address :from) (address :to))`
  * Checks whether a transfer is permitted from one address to the other one. Fails if it is not.
* `assertReceiver (address)`
  * Fails if address is not whitelisted or it is blacklisted.
* `assertReceivers(list address)`
  * Fails if any address in the list is not whitelisted or is blacklisted.

This contract implements regulatory service for the **Holdings** contract.

## Holdings contract

Approximate storage structure:
```haskell
type Storage = StorageSkeleton StorageFields

data StorageSkeleton fields = StorageSkeleton
  { ledger :: BigMap Address LedgerValue
  , fields :: fields
  }

data StorageFields = StorageFields
  { tokenMeta :: TokenMeta
  , mbSafelistAddress :: Maybe Address
  , owner :: Address
  , admin :: Address
  , mbNewAdmin :: Maybe Address
  , paused :: Bool
  , transferable :: Bool
  , totalSupply :: Natural
  }

data TokenMeta = TokenMeta
  { tmName :: MText
  , tmSymbol :: MText
  , tmId :: MText
  }
```

Required `Holdings` entrypoints:
* `setName (string :newName)`
  * Description: updates `name` field.
  * Constraints: `onlyAdmin` `isNotPaused`.
* `setSymbol (string :newSymbol)`
  * Description: updates `symbol` field.
  * Constraints: `onlyAdmin` `isNotPaused`.
* `setSafelistAddress ((option address) :newMbSafelistAddress)`
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
  * Safelist constraints: `assertTransfer` passes.
* `approve (address :spender, nat :value)`
  * Description: approve given amout of token to be spent by given address from `sender`.
  * Constraints: `isNotPaused`.
  * Safelist constraints: `assertReceivers` passes for `spender` and `SENDER`.
* `getAllowance (view (address :owner, address :spender) natural)`
  * Description: returns approval amount for two given addresses.
* `getBalance (view (address :owner) natural)`
  * Description: returns balance of given address.
* `getTotalSupply (view unit nat)`
  * Description: return total supply (`totalSupply = totalMinted - totalBurned`).
* `mint (address :investor, nat: value)`
  * Description: mint given amount of tokens to given address.
  * Constraints: `onlyAdmin`, `isNotPaused`.
  * Safelist constraints: `assertReceiver` passes for `investor`
* `burn (address :investor, nat :value)`
  * Description: burn amount given amount of tokens from given address.
  * Constraints: `onlyAdmin`, `isNotPaused`
* `burnAll ()`
  * Description: burn all tokens and remove all approvals.
  * Constraints: `onlyAdmin`, `isNotPaused`.
* `setPause (bool :value)`
  * Description: pause/unpause the contract. When paused no transactions should be possible on this contract.
  * Constraints: `onlyAdmin`.
* `setTransferable (bool :value)`
  * Description: update `transferable` flag. When this flag is set to false, transfers should not be possible.
  * Constraints: `onlyAdmin`.

Additional `Holdings` entrypoints used for testing:
* `getTokenMeta (view () (string, string, string))`
  * Description: returns current token meta information (its name, symbol and id).
