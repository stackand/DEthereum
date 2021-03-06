# DEthereum 

DEthereum is the Delphi integration library for Ethereum, it allows you:
- interact with Ethereum nodes like geth/parity using RPC
- generating Delphi classes from solidity/ABI
- generating ABI from runtime TEthereumContrtact class

## Supported RPC calls:
- web3_*
- eth_*
- db_*
- ssh_* (partially) 
- personal_*
- tx_poll_*
- miner_*
- admin_*
- debug_* (not yet supported)

## Supported types map for contracts:
|Delphi|Solidity
|-|-
|Byte|uint8|
|Integer|int32|
|UInt32|uint32|
|Int64|int64|
|Uint64|uint64|
|TByteDynArray|bytes32, address, any|
|Boolean|bool|
|String|string|

## Requirements 
- Embarcadero® Delphi 10.2 (on later version not tested)

