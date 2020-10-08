# Holdings

**Code revision:** [054e7d9](https://github.com/serokell/tezos-globacap/blob/054e7d9fc4a351e3f36dbf2bd6b20b51c8217c3e) *(Thu Oct 8 20:40:51 2020 +0300)*



This contract is used to distribute the token, it is optionally regulated by the Safelist contract.

## Storage

<a name="storage-Holdings-storage"></a>

---

### `Holdings storage`

Root datatype for Holdings contract storage type. It contains two `big_map`s: 
* `ledger` - stores addresses balances.
* `approvals` - stores approvals amounts.

Apart from that it has `fields` which store various additional information about contract state. See `Holdings storage fields` type.

**Structure:** 
  * ***ledger*** :[`BigMap`](#types-BigMap) [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen) (***balance*** : [`Natural`](#types-Natural))
  * ***approvals*** :[`BigMap`](#types-BigMap) (***owner*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen), ***spender*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)) [`Natural`](#types-Natural)
  * ***fields*** :[`Holdings storage fields`](#types-Holdings-storage-fields)

**Final Michelson representation:** `pair (big_map address nat) (pair (big_map (pair address address) nat) (pair (pair (pair (pair string (pair string string)) (option address)) (pair address address)) (pair (pair (option address) bool) (pair bool nat))))`



## Entrypoints

<a name="entrypoints-setName"></a>

---

### `setName`

Change token name.

**Argument:** 
  + **In Haskell:** ***newName*** : [`Text`](#types-Text)
  + **In Michelson:** `(string :newName)`
    + **Example:** <span id="example-id">`"hello"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `setName` entrypoint passing the constructed argument.
</details>
<p>



**Pausable:** Cannot be executed when token operations are paused.

**Possible errors:**
* [`SenderIsNotAdmin`](#errors-SenderIsNotAdmin) — Entrypoint executed not by its administrator.

* [`TokenOperationsArePaused`](#errors-TokenOperationsArePaused) — Token functionality (`transfer` and similar entrypoints) is suspended.



<a name="entrypoints-setSymbol"></a>

---

### `setSymbol`

Change token symbol.

**Argument:** 
  + **In Haskell:** ***newSymbol*** : [`Text`](#types-Text)
  + **In Michelson:** `(string :newSymbol)`
    + **Example:** <span id="example-id">`"hello"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `setSymbol` entrypoint passing the constructed argument.
</details>
<p>



**Pausable:** Cannot be executed when token operations are paused.

**Possible errors:**
* [`SenderIsNotAdmin`](#errors-SenderIsNotAdmin) — Entrypoint executed not by its administrator.

* [`TokenOperationsArePaused`](#errors-TokenOperationsArePaused) — Token functionality (`transfer` and similar entrypoints) is suspended.



<a name="entrypoints-setSafelistAddress"></a>

---

### `setSafelistAddress`

Change optional Safelist contract address.

**Argument:** 
  + **In Haskell:** ***newMbSafelistAddress*** : [`Maybe`](#types-Maybe) [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  + **In Michelson:** `(option :newMbSafelistAddress address)`
    + **Example:** <span id="example-id">`Some "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `setSafelistAddress` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender is not the contract owner.

* [`InvalidSafelistAddr`](#errors-InvalidSafelistAddr) — New safelist address doesn't have required entrypoint



<a name="entrypoints-transferAdminRights"></a>

---

### `transferAdminRights`

Transfer admin rights to the new address.

**Argument:** 
  + **In Haskell:** ***newAdmin*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  + **In Michelson:** `(address :newAdmin)`
    + **Example:** <span id="example-id">`"KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `transferAdminRights` entrypoint passing the constructed argument.
</details>
<p>



**Pausable:** Cannot be executed when token operations are paused.

**Possible errors:**
* [`SenderIsNotOwner`](#errors-SenderIsNotOwner) — Sender is not the contract owner.

* [`TokenOperationsArePaused`](#errors-TokenOperationsArePaused) — Token functionality (`transfer` and similar entrypoints) is suspended.



<a name="entrypoints-acceptAdminRights"></a>

---

### `acceptAdminRights`

Accept admin rights by the new admin.

**Argument:** 
  + **In Haskell:** [`()`](#types-lparenrparen)
  + **In Michelson:** `unit`
    + **Example:** <span id="example-id">`Unit`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `acceptAdminRights` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`NotInTransferAdminRightsMode`](#errors-NotInTransferAdminRightsMode) — Cannot accept admin rights before transfer process has been initiated by calling transferAdminRights entrypoint.

* [`SenderIsNotNewAdmin`](#errors-SenderIsNotNewAdmin) — Cannot accept admin rights because the sender address is different from the address passed to the transferadminRights entrypoint previously.



<a name="entrypoints-isAdmin"></a>

---

### `isAdmin`

Check whether address is admin. Returns `True` if the address is the admin.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen) [`Bool`](#types-Bool)
  + **In Michelson:** `(pair (address %viewParam) (contract %viewCallbackTo bool))`
    + **Example:** <span id="example-id">`Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `isAdmin` entrypoint passing the constructed argument.
</details>
<p>





<a name="entrypoints-transfer"></a>

---

### `transfer`

Transfers tokens between two given accounts.

This entrypoint serves multiple purposes:
* When called with `"from"` account equal to the transaction sender, we assume that
the user transfers their own money and this does not require approval.
* Otherwise, the transaction sender must be previously authorized to transfer at least the requested number of tokens from the `"from"` account using the `approve` entrypoint.
In this case current number of tokens that sender is allowed to withdraw from the `"from"` address is decreased by the number of transferred tokens.



**Argument:** 
  + **In Haskell:** (***from*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen), ***to*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen), ***value*** : [`Natural`](#types-Natural))
  + **In Michelson:** `(pair (address :from) (pair (address :to) (nat :value)))`
    + **Example:** <span id="example-id">`Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" (Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" 0)`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `transfer` entrypoint passing the constructed argument.
</details>
<p>



**Pausable:** Cannot be executed when token operations are paused.

**Possible errors:**
* [`NonTransferable`](#errors-NonTransferable) — Transferable flag is false.

* [`InvalidSafelistAddr`](#errors-InvalidSafelistAddr) — New safelist address doesn't have required entrypoint

* [`TokenOperationsArePaused`](#errors-TokenOperationsArePaused) — Token functionality (`transfer` and similar entrypoints) is suspended.

* [`NotEnoughAllowance`](#errors-NotEnoughAllowance) — Not enough funds allowance to perform the operation.

* [`NotEnoughBalance`](#errors-NotEnoughBalance) — Not enough funds to perform the operation.



<a name="entrypoints-seize"></a>

---

### `seize`

Forcibly send given amount of tokens from one address to another.

**Argument:** 
  + **In Haskell:** (***from*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen), ***to*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen), ***value*** : [`Natural`](#types-Natural))
  + **In Michelson:** `(pair (address :from) (pair (address :to) (nat :value)))`
    + **Example:** <span id="example-id">`Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" (Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" 0)`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `seize` entrypoint passing the constructed argument.
</details>
<p>



**Pausable:** Cannot be executed when token operations are paused.

**Possible errors:**
* [`SenderIsNotAdmin`](#errors-SenderIsNotAdmin) — Entrypoint executed not by its administrator.

* [`TokenOperationsArePaused`](#errors-TokenOperationsArePaused) — Token functionality (`transfer` and similar entrypoints) is suspended.

* [`NonTransferable`](#errors-NonTransferable) — Transferable flag is false.

* [`InvalidSafelistAddr`](#errors-InvalidSafelistAddr) — New safelist address doesn't have required entrypoint

* [`NotEnoughBalance`](#errors-NotEnoughBalance) — Not enough funds to perform the operation.



<a name="entrypoints-approve"></a>

---

### `approve`

When called with `(address :spender, nat :value)`
parameters allows `spender` account to withdraw from the sender, multiple times,
up to the `value` amount.
Each call of `transfer` entrypoint decreases the allowance amount on the transferred amount of
tokens, unless `transfer` is called with `from` account equal to sender, in which case allowance
is always ignored.
In other terms self-approval, where 'from` is equal to sender, is redundant and will never be consumed by a 'transfer'.

If this entrypoint is called again, it overwrites the current allowance with `value`.

**DISCLAIMER**: this suffers from an [attack vector](https://docs.google.com/document/d/1YLPtQxZu1UAvO9cZ1O2RPXBbT0mooh4DYKjA_jp-RLM),
that is a known issue of `ERC20` and, as a consequence, of `FA1.2` (which is
based on it).
It is not safe to change the approval from a non-zero value to a non-zero value.
This is the reason why performing such a change directly is not allowed by the contract.
However this is not enough on its own, a token holder that intends to
safely change the allowance for `X` to `K` token must:
1. read the current allowance `M` for `X` from the latest transaction `S`.
2. send a transaction `T` that sets the allowance to `0`.
3. wait for the blockchain to confirm that `T` is included.
4. scan all transactions between `S` and `T`.
5. calculate the allowance `N <= M` spent by `X` in those transactions.
6. set the allowance to `K - N` iff `N < K`.


**Argument:** 
  + **In Haskell:** (***spender*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen), ***value*** : [`Natural`](#types-Natural))
  + **In Michelson:** `(pair (address :spender) (nat :value))`
    + **Example:** <span id="example-id">`Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" 0`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `approve` entrypoint passing the constructed argument.
</details>
<p>



**Pausable:** Cannot be executed when token operations are paused.

**Possible errors:**
* [`InvalidSafelistAddr`](#errors-InvalidSafelistAddr) — New safelist address doesn't have required entrypoint

* [`TokenOperationsArePaused`](#errors-TokenOperationsArePaused) — Token functionality (`transfer` and similar entrypoints) is suspended.

* [`UnsafeAllowanceChange`](#errors-UnsafeAllowanceChange) — Allowance change from non-zero value to non-zero value is performed.



<a name="entrypoints-getAllowance"></a>

---

### `getAllowance`

Returns the approval value between two given addresses.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) (***owner*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen), ***spender*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)) [`Natural`](#types-Natural)
  + **In Michelson:** `(pair (pair %viewParam (address :owner) (address :spender)) (contract %viewCallbackTo nat))`
    + **Example:** <span id="example-id">`Pair (Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB") "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `getAllowance` entrypoint passing the constructed argument.
</details>
<p>





<a name="entrypoints-getBalance"></a>

---

### `getBalance`

Returns the balance of the address in the ledger.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) (***owner*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)) [`Natural`](#types-Natural)
  + **In Michelson:** `(pair (address :owner %viewParam) (contract %viewCallbackTo nat))`
    + **Example:** <span id="example-id">`Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `getBalance` entrypoint passing the constructed argument.
</details>
<p>





<a name="entrypoints-getTotalSupply"></a>

---

### `getTotalSupply`

Returns total number of tokens.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) [`()`](#types-lparenrparen) [`Natural`](#types-Natural)
  + **In Michelson:** `(pair (unit %viewParam) (contract %viewCallbackTo nat))`
    + **Example:** <span id="example-id">`Pair Unit "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `getTotalSupply` entrypoint passing the constructed argument.
</details>
<p>





<a name="entrypoints-mint"></a>

---

### `mint`

Produces tokens on the account associated with the given address.

**Argument:** 
  + **In Haskell:** (***to*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen), ***value*** : [`Natural`](#types-Natural))
  + **In Michelson:** `(pair (address :to) (nat :value))`
    + **Example:** <span id="example-id">`Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" 0`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `mint` entrypoint passing the constructed argument.
</details>
<p>



**Authorization:** The sender has to be `administrator`.

**Pausable:** Cannot be executed when token operations are paused.

**Possible errors:**
* [`TokenOperationsArePaused`](#errors-TokenOperationsArePaused) — Token functionality (`transfer` and similar entrypoints) is suspended.

* [`InvalidSafelistAddr`](#errors-InvalidSafelistAddr) — New safelist address doesn't have required entrypoint

* [`SenderIsNotAdmin`](#errors-SenderIsNotAdmin) — Entrypoint executed not by its administrator.



<a name="entrypoints-burn"></a>

---

### `burn`

Destroys the given amount of tokens on the account associated with the given address.

**Argument:** 
  + **In Haskell:** (***from*** : [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen), ***value*** : [`Natural`](#types-Natural))
  + **In Michelson:** `(pair (address :from) (nat :value))`
    + **Example:** <span id="example-id">`Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" 0`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `burn` entrypoint passing the constructed argument.
</details>
<p>



**Authorization:** The sender has to be `administrator`.

**Pausable:** Cannot be executed when token operations are paused.

**Possible errors:**
* [`TokenOperationsArePaused`](#errors-TokenOperationsArePaused) — Token functionality (`transfer` and similar entrypoints) is suspended.

* [`SenderIsNotAdmin`](#errors-SenderIsNotAdmin) — Entrypoint executed not by its administrator.

* [`NotEnoughBalance`](#errors-NotEnoughBalance) — Not enough funds to perform the operation.



<a name="entrypoints-burnAll"></a>

---

### `burnAll`

Destroy all tokens and allowances.

**Argument:** 
  + **In Haskell:** [`()`](#types-lparenrparen)
  + **In Michelson:** `unit`
    + **Example:** <span id="example-id">`Unit`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `burnAll` entrypoint passing the constructed argument.
</details>
<p>



**Pausable:** Cannot be executed when token operations are paused.

**Possible errors:**
* [`SenderIsNotAdmin`](#errors-SenderIsNotAdmin) — Entrypoint executed not by its administrator.

* [`TokenOperationsArePaused`](#errors-TokenOperationsArePaused) — Token functionality (`transfer` and similar entrypoints) is suspended.



<a name="entrypoints-setPause"></a>

---

### `setPause`

This entrypoint pauses operations when the parameter is `True`,
and resumes them when the parameter is `False`. During the pause,
no contract can perform `transfer` or `approval` operations.


**Argument:** 
  + **In Haskell:** ***value*** : [`Bool`](#types-Bool)
  + **In Michelson:** `(bool :value)`
    + **Example:** <span id="example-id">`True`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `setPause` entrypoint passing the constructed argument.
</details>
<p>



**Authorization:** The sender has to be `administrator`.

**Possible errors:**
* [`SenderIsNotAdmin`](#errors-SenderIsNotAdmin) — Entrypoint executed not by its administrator.



<a name="entrypoints-setTransferable"></a>

---

### `setTransferable`

Change transferable flag.

**Argument:** 
  + **In Haskell:** ***value*** : [`Bool`](#types-Bool)
  + **In Michelson:** `(bool :value)`
    + **Example:** <span id="example-id">`True`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `setTransferable` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`SenderIsNotAdmin`](#errors-SenderIsNotAdmin) — Entrypoint executed not by its administrator.



<a name="entrypoints-getTokenMeta"></a>

---

### `getTokenMeta`

Get token meta data: name, symbol and id.

**Argument:** 
  + **In Haskell:** [`View`](#types-View) [`()`](#types-lparenrparen) [`TokenMeta`](#types-TokenMeta)
  + **In Michelson:** `(pair (unit %viewParam) (contract %viewCallbackTo (pair (string %tmName) (pair (string %tmSymbol) (string %tmId)))))`
    + **Example:** <span id="example-id">`Pair Unit "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `getTokenMeta` entrypoint passing the constructed argument.
</details>
<p>









# Definitions

## Types

<a name="types-lparenrparen"></a>

---

### `()`

Unit primitive.

**Structure:** ()

**Final Michelson representation:** `unit`



<a name="types-lparenacomma-brparen"></a>

---

### `(a, b)`

Pair primitive.

**Final Michelson representation (example):** `(Integer,Natural)` = `pair int nat`



<a name="types-lparenacomma-bcomma-crparen"></a>

---

### `(a, b, c)`

Tuple of size 3.

**Final Michelson representation (example):** `(Integer,Natural,MText)` = `pair int (pair nat string)`



<a name="types-Address-lparenno-entrypointrparen"></a>

---

### `Address (no entrypoint)`

This is similar to Michelson Address, but does not retain entrypoint name if it refers to a contract.

**Final Michelson representation:** `address`



<a name="types-BigMap"></a>

---

### `BigMap`

BigMap primitive.

**Final Michelson representation (example):** `BigMap Integer Natural` = `big_map int nat`



<a name="types-Bool"></a>

---

### `Bool`

Bool primitive.

**Final Michelson representation:** `bool`



<a name="types-Contract"></a>

---

### `Contract`

Contract primitive with given type of parameter.

**Final Michelson representation (example):** `ContractRef Integer` = `contract int`



<a name="types-Holdings-storage-fields"></a>

---

### `Holdings storage fields`

Additional contract fields that define the current contract state. It stores meta-information about token (see `TokenMeta` type), information about the `owner` and the current `admin` of the contract. Additionally it stores an optional address for currently used Safelist contract, the `totalSupply` amount, and the `paused` and `transferable` flags.

**Structure:** 
  * ***tokenMeta*** :[`TokenMeta`](#types-TokenMeta)
  * ***mbSafelistAddress*** :[`Maybe`](#types-Maybe) [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  * ***owner*** :[`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  * ***admin*** :[`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  * ***mbNewAdmin*** :[`Maybe`](#types-Maybe) [`Address (no entrypoint)`](#types-Address-lparenno-entrypointrparen)
  * ***paused*** :[`Bool`](#types-Bool)
  * ***transferable*** :[`Bool`](#types-Bool)
  * ***totalSupply*** :[`Natural`](#types-Natural)

**Final Michelson representation:** `pair (pair (pair (pair string (pair string string)) (option address)) (pair address address)) (pair (pair (option address) bool) (pair bool nat))`



<a name="types-Integer"></a>

---

### `Integer`

Signed number.

**Final Michelson representation:** `int`



<a name="types-Maybe"></a>

---

### `Maybe`

Option primitive.

**Final Michelson representation (example):** `Maybe Integer` = `option int`



<a name="types-Named-entry"></a>

---

### `Named entry`

Some entries have names for clarity.

In resulting Michelson names may be mapped to annotations.

**Final Michelson representation (example):** `number: Integer` = `int`



<a name="types-Natural"></a>

---

### `Natural`

Unsigned number.

**Final Michelson representation:** `nat`



<a name="types-Text"></a>

---

### `Text`

Michelson string.

This has to contain only ASCII characters with codes from [32; 126] range; additionally, newline feed character is allowed.

**Final Michelson representation:** `string`



<a name="types-TokenMeta"></a>

---

### `TokenMeta`

Meta information about token.

**Structure:** 
  * ***name*** :[`Text`](#types-Text)
  * ***symbol*** :[`Text`](#types-Text)
  * ***id*** :[`Text`](#types-Text)

**Final Michelson representation:** `pair string (pair string string)`



<a name="types-View"></a>

---

### `View`

`View a r` accepts an argument of type `a` and callback contract which accepts `r` and returns result via calling that contract.
Read more in [A1 conventions document](https://gitlab.com/tzip/tzip/-/blob/c42e3f0f5e73669e84e615d69bee73281572eb0a/proposals/tzip-4/tzip-4.md#view-entrypoints).

**Structure (example):** `View () Integer` = 
[`()`](#types-lparenrparen)
[`ContractRef`](#types-Contract) [`Integer`](#types-Integer)

**Final Michelson representation (example):** `View () Integer` = `pair unit (contract int)`



## Errors

Our contract implies the possibility of error scenarios, this section enlists
all values which the contract can produce via calling `FAILWITH` instruction
on them. In case of error, no changes to contract state will be applied.

Each entrypoint also contains a list of errors which can be raised during its
execution; only for no-throw entrypoints this list will be omitted.
Errors in these lists are placed in the order in which the corresponding
properties are checked unless the opposite is specified. I.e., if for a
given entrypoint call two different errors may take place, the one which
appears in the list first will be thrown.

Most of the errors are represented according to the same
`(error tag, error argument)` pattern. See the list of errors below
for details.

We distinquish several error classes:
+ **Action exception**: given action cannot be performed with
  regard to the current contract state.

  Examples: "insufficient balance", "wallet does not exist".

  If you are implementing a middleware, such errors should be propagated to
  the client.

+ **Bad argument**: invalid argument supplied to the entrypoint.

  Examples: entrypoint accepts a natural number from `0-3` range, and you
  supply `5`.

  If you are implementing a middleware, you should care about not letting
  such errors happen.

+ **Internal**: contract-internal error.

  In ideal case, such errors should not take place, but still, make sure
  that you are ready to handle them. They can signal either invalid contract
  deployment or a bug in contract implementation.

  If an internal error is thrown, please report it to the author of this contract.


<a name="errors-InternalError"></a>

---

### `InternalError`

**Class:** Internal

**Fires if:** Some internal error occured.

**Representation:** Textual error message, see [`Text`](#types-Text).

<a name="errors-InvalidSafelistAddr"></a>

---

### `InvalidSafelistAddr`

**Class:** Bad argument

**Fires if:** New safelist address doesn't have required entrypoint

**Representation:** `("InvalidSafelistAddr", <error argument>)`.

Provided error argument will be of type [`Text`](#types-Text).

<a name="errors-NonTransferable"></a>

---

### `NonTransferable`

**Class:** Bad argument

**Fires if:** Transferable flag is false. Transfers are prohibited.

**Representation:** `("NonTransferable", ())`.

<a name="errors-NotEnoughAllowance"></a>

---

### `NotEnoughAllowance`

**Class:** Action exception

**Fires if:** Not enough funds allowance to perform the operation.

**Representation:** `("NotEnoughAllowance", <error argument>)`.

Provided error argument will be of type (***required*** : [`Natural`](#types-Natural), ***present*** : [`Natural`](#types-Natural)).

<a name="errors-NotEnoughBalance"></a>

---

### `NotEnoughBalance`

**Class:** Action exception

**Fires if:** Not enough funds to perform the operation.

**Representation:** `("NotEnoughBalance", <error argument>)`.

Provided error argument will be of type (***required*** : [`Natural`](#types-Natural), ***present*** : [`Natural`](#types-Natural)).

<a name="errors-NotInTransferAdminRightsMode"></a>

---

### `NotInTransferAdminRightsMode`

**Class:** Action exception

**Fires if:** Cannot accept admin rights before transfer process has been initiated by calling transferAdminRights entrypoint.

**Representation:** `("NotInTransferAdminRightsMode", ())`.

<a name="errors-SenderIsNotAdmin"></a>

---

### `SenderIsNotAdmin`

**Class:** Action exception

**Fires if:** Entrypoint executed not by its administrator.

**Representation:** `("SenderIsNotAdmin", ())`.

<a name="errors-SenderIsNotNewAdmin"></a>

---

### `SenderIsNotNewAdmin`

**Class:** Bad argument

**Fires if:** Cannot accept admin rights because the sender address is different from the address passed to the transferadminRights entrypoint previously.

**Representation:** `("SenderIsNotNewAdmin", ())`.

<a name="errors-SenderIsNotOwner"></a>

---

### `SenderIsNotOwner`

**Class:** Action exception

**Fires if:** Sender is not the contract owner.

**Representation:** `("SenderIsNotOwner", ())`.

<a name="errors-TokenOperationsArePaused"></a>

---

### `TokenOperationsArePaused`

**Class:** Action exception

**Fires if:** Token functionality (`transfer` and similar entrypoints) is suspended.

**Representation:** `("TokenOperationsArePaused", ())`.

<a name="errors-UnsafeAllowanceChange"></a>

---

### `UnsafeAllowanceChange`

**Class:** Action exception

**Fires if:** Allowance change from non-zero value to non-zero value is performed. This contract does not allow such an update, see the [corresponding attack vector](https://docs.google.com/document/d/1YLPtQxZu1UAvO9cZ1O2RPXBbT0mooh4DYKjA_jp-RLM) for explanation.

**Representation:** `("UnsafeAllowanceChange", <error argument>)`.

Provided error argument will be of type [`Natural`](#types-Natural) and stand for the previous value of approval.
