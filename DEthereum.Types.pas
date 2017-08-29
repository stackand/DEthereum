unit DEthereum.Types;

interface

uses
  System.Types;

const
  eth_hex = '0x';
  eth_len = 64;

type
  TEth_BlockNumber = (ethbnCustom, ethbnEearliest, ethbnLatest, ethbnPending);

  TEth_BlockNumberHelper = record helper for TEth_BlockNumber
    function ToString(BlockNumberCustom: Int64): String;
  end;

  TEth_ErrorClass = class
  private
    FCode: Integer;
    FMessage: String;
  public
    property code: Integer read FCode write FCode;
    property &message: String read FMessage write FMessage;
  end;

  TEth_TransactionClass = class
  private
    FBlockHash: String;
    FBlockNumber: String;
    FFrom: String;
    FGas: String;
    FGasPrice: String;
    FHash: String;
    FInput: String;
    FNonce: String;
    FR: String;
    FS: String;
    FTransactionIndex: String;
    FV: String;
    FValue: String;
  public
    property blockHash: String read FBlockHash write FBlockHash;
    property blockNumber: String read FBlockNumber write FBlockNumber;
    property from: String read FFrom write FFrom;
    property gas: String read FGas write FGas;
    property gasPrice: String read FGasPrice write FGasPrice;
    property hash: String read FHash write FHash;
    property input: String read FInput write FInput;
    property nonce: String read FNonce write FNonce;
    property r: String read FR write FR;
    property s: String read FS write FS;
    property transactionIndex: String read FTransactionIndex write FTransactionIndex;
    property v: String read FV write FV;
    property value: String read FValue write FValue;
  end;

  TEth_BlockClass = class
  private
    FDifficulty: String;
    FExtraData: String;
    FGasLimit: String;
    FGasUsed: String;
    FHash: String;
    FLogsBloom: String;
    FMiner: String;
    FMixHash: String;
    FNonce: String;
    FNumber: String;
    FParentHash: String;
    FReceiptsRoot: String;
    FSha3Uncles: String;
    FSize: String;
    FStateRoot: String;
    FTimestamp: String;
    FTotalDifficulty: String;
    FTransactions: TArray<TEth_TransactionClass>;
    FTransactionsRoot: String;
    //FUncles: TArray<String>;
  public
    property difficulty: String read FDifficulty write FDifficulty;
    property extraData: String read FExtraData write FExtraData;
    property gasLimit: String read FGasLimit write FGasLimit;
    property gasUsed: String read FGasUsed write FGasUsed;
    property hash: String read FHash write FHash;
    property logsBloom: String read FLogsBloom write FLogsBloom;
    property miner: String read FMiner write FMiner;
    property mixHash: String read FMixHash write FMixHash;
    property nonce: String read FNonce write FNonce;
    property number: String read FNumber write FNumber;
    property parentHash: String read FParentHash write FParentHash;
    property receiptsRoot: String read FReceiptsRoot write FReceiptsRoot;
    property sha3Uncles: String read FSha3Uncles write FSha3Uncles;
    property size: String read FSize write FSize;
    property stateRoot: String read FStateRoot write FStateRoot;
    property timestamp: String read FTimestamp write FTimestamp;
    property totalDifficulty: String read FTotalDifficulty write FTotalDifficulty;
    property transactions: TArray<TEth_TransactionClass> read FTransactions write FTransactions;
    property transactionsRoot: String read FTransactionsRoot write FTransactionsRoot;
    //property uncles: TArray<String> read FUncles write FUncles;
    destructor Destroy; override;
  end;

  TEth_TransactionReceiptClass = class
  private
    FBlockHash: String;
    FBlockNumber: String;
    FContractAddress: String;
    FCumulativeGasUsed: String;
    FFrom: String;
    FGasUsed: String;
    FLogs: TArray<TEth_TransactionClass>;
    FLogsBloom: String;
    FRoot: String;
    FTo: String;
    FTransactionHash: String;
    FTransactionIndex: String;
  public
    property blockHash: String read FBlockHash write FBlockHash;
    property blockNumber: String read FBlockNumber write FBlockNumber;
    property contractAddress: String read FContractAddress write FContractAddress;
    property cumulativeGasUsed: String read FCumulativeGasUsed write FCumulativeGasUsed;
    property from: String read FFrom write FFrom;
    property gasUsed: String read FGasUsed write FGasUsed;
    property logs: TArray<TEth_TransactionClass> read FLogs write FLogs;
    property logsBloom: String read FLogsBloom write FLogsBloom;
    property root: String read FRoot write FRoot;
    property &to: String read FTo write FTo;
    property transactionHash: String read FTransactionHash write FTransactionHash;
    property transactionIndex: String read FTransactionIndex write FTransactionIndex;
    destructor Destroy; override;
  end;

  TEth_FilterChangeClass = class
  private
    FAddress: String;
    FBlockHash: String;
    FBlockNumber: String;
    FData: String;
    FLogIndex: String;
    FTopics: TArray<String>;
    FTransactionHash: String;
    FTransactionIndex: String;
  public
    property address: String read FAddress write FAddress;
    property blockHash: String read FBlockHash write FBlockHash;
    property blockNumber: String read FBlockNumber write FBlockNumber;
    property data: String read FData write FData;
    property logIndex: String read FLogIndex write FLogIndex;
    property topics: TArray<String> read FTopics write FTopics;
    property transactionHash: String read FTransactionHash write FTransactionHash;
    property transactionIndex: String read FTransactionIndex write FTransactionIndex;
  end;

  TEth_getFilterChangesClass = class
  private
    FId: Int64;
    FJsonrpc: String;
    FResult: TArray<TEth_FilterChangeClass>;
  public
    property id: Int64 read FId write FId;
    property jsonrpc: String read FJsonrpc write FJsonrpc;
    property result: TArray<TEth_FilterChangeClass> read FResult write FResult;
    destructor Destroy; override;
  end;

  TEth_syncingClass = class
  private
    FCurrentBlock: String;
    FHighestBlock: String;
    FStartingBlock: String;
  public
    property currentBlock: String read FCurrentBlock write FCurrentBlock;
    property highestBlock: String read FHighestBlock write FHighestBlock;
    property startingBlock: String read FStartingBlock write FStartingBlock;
  end;

  TEth_StringArrayResultClass = class
  private
    FId: Int64;
    FJsonrpc: String;
    FResult: TArray<String>;
  public
    property id: Int64 read FId write FId;
    property jsonrpc: String read FJsonrpc write FJsonrpc;
    property result: TArray<String> read FResult write FResult;
  end;

  TTxpool_statusClass = class
  private
    FPending: String;
    FQueued: String;
  public
    property pending: String read FPending write FPending;
    property queued: String read FQueued write FQueued;
  end;


  TAdmin_nodeinfo_Protocols_EthClass = class
  private
    FDifficulty: Int64;
    FGenesis: String;
    FHead: String;
    FNetwork: Int64;
  public
    property difficulty: Int64 read FDifficulty write FDifficulty;
    property genesis: String read FGenesis write FGenesis;
    property head: String read FHead write FHead;
    property network: Int64 read FNetwork write FNetwork;
  end;

  TAdmin_nodeinfo_ProtocolsClass = class
  private
    FEth: TAdmin_nodeinfo_Protocols_EthClass;
  public
    property eth: TAdmin_nodeinfo_Protocols_EthClass read FEth write FEth;
    constructor Create;
    destructor Destroy; override;
  end;

  TAdmin_nodeinfo_PortsClass = class
  private
    FDiscovery: Int64;
    FListener: Int64;
  public
    property discovery: Int64 read FDiscovery write FDiscovery;
    property listener: Int64 read FListener write FListener;
  end;

  TAdmin_nodeInfoClass = class
  private
    FEnode: String;
    FId: String;
    FIp: String;
    FListenAddr: String;
    FName: String;
    FPorts: TAdmin_nodeinfo_PortsClass;
    FProtocols: TAdmin_nodeinfo_ProtocolsClass;
  public
    property enode: String read FEnode write FEnode;
    property id: String read FId write FId;
    property ip: String read FIp write FIp;
    property listenAddr: String read FListenAddr write FListenAddr;
    property &name: String read FName write FName;
    property ports: TAdmin_nodeinfo_PortsClass read FPorts write FPorts;
    property protocols: TAdmin_nodeinfo_ProtocolsClass read FProtocols write FProtocols;
    constructor Create;
    destructor Destroy; override;
  end;


  TAdmin_nodePeers_NetworkClass = class
  private
    FLocalAddress: String;
    FRemoteAddress: String;
  public
    property localAddress: String read FLocalAddress write FLocalAddress;
    property remoteAddress: String read FRemoteAddress write FRemoteAddress;
  end;

  TAdmin_nodePeersClass = class
  private
    FCaps: TArray<String>;
    FId: String;
    FName: String;
    FNetwork: TAdmin_nodePeers_NetworkClass;
    FProtocols: TAdmin_nodeinfo_ProtocolsClass;
  public
    property caps: TArray<String> read FCaps write FCaps;
    property id: String read FId write FId;
    property &name: String read FName write FName;
    property network: TAdmin_nodePeers_NetworkClass read FNetwork write FNetwork;
    property protocols: TAdmin_nodeinfo_ProtocolsClass read FProtocols write FProtocols;
    constructor Create;
    destructor Destroy; override;
  end;


function eth_hexToInt(const s: String): Int64;
function eth_hexToUInt(const s: String): UInt64;
function eth_hexToStr(s: String): String;
function eth_hexToBytes(s: String): TByteDynArray;
function eth_booleanToHex(b: Boolean; HexPrefix: Boolean = True; Pad: Integer = 0): String;
function eth_intToHex(i: Int64; HexPrefix: Boolean = True; Pad: Integer = 0): String;
function eth_uintToHex(i: UInt64; HexPrefix: Boolean = True; Pad: Integer = 0): String;
function eth_strToHex(const s: String; HexPrefix: Boolean = True; Pad: Integer = 0): String;
function eth_bytesToStr(const b: TByteDynArray): String;
function eth_bytesToHex(const b: TByteDynArray; HexPrefix: Boolean = True; Pad: Integer = 0): String;

function iif(const ACondition: boolean; const BranchTRUE, BranchFALSE: variant): variant;

implementation

uses
  System.StrUtils, System.SysUtils, System.Classes;


function eth_hexToStr(s: String): String;
var
  i: Integer;
begin
  i := 1;
  Result := '';
  s := StringReplace(s, eth_hex, '$', []);
  while i < s.Length  do
    begin
      Result := Result + Char(StrToInt('$' + Copy(s, i, 2)));
      Inc(i, 2);
    end;
end;

function eth_hexToBytes(s: String): TByteDynArray;
begin
  s := eth_hexToStr(s);
  SetLength(Result, Length(s));
  System.Move(s[1], Result[0], Length(s));
end;

function eth_booleanToHex(b: Boolean; HexPrefix: Boolean = True; Pad: Integer = 0): String;
begin
  if b
    then Result := eth_intToHex(1, HexPrefix, Pad)
    else Result := eth_intToHex(0, HexPrefix, Pad);
end;

function eth_intToHex(i: Int64; HexPrefix: Boolean; Pad: Integer): String;
begin
  Result := IntToHex(i, 2);

  if Pad <> 0 then
    while Length(Result) mod abs(Pad * 2) > 0 do
      if Pad > 0
        then Result := Result + '0'
        else Result := '0' + Result;

  if HexPrefix then
    Result := eth_hex + Result;
end;

function eth_uintToHex(i: UInt64; HexPrefix: Boolean = True; Pad: Integer = 0): String;
begin
  Result := IntToHex(i, 2);

  if Pad <> 0 then
    while Length(Result) mod abs(Pad * 2) > 0 do
      if Pad > 0
        then Result := Result + '0'
        else Result := '0' + Result;

  if HexPrefix then
    Result := eth_hex + Result;
end;

function eth_hexToInt(const s: String): Int64;
begin
  Result := StrToInt64('$' + StringReplace(s, eth_hex, '', []));
end;

function eth_hexToUInt(const s: String): UInt64;
begin
  Result := StrToUInt64('$' + StringReplace(s, eth_hex, '', []));
end;

function eth_strToHex(const s: String; HexPrefix: Boolean; Pad: Integer): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
    Result := Result + IntToHex(Ord(s[i]), 2);

  if Pad <> 0 then
    while Length(Result) mod abs(Pad * 2) > 0 do
      if Pad > 0
        then Result := Result + '0'
        else Result := '0' + Result;

  if HexPrefix then
    Result := eth_hex + Result;
end;

function eth_bytesToStr(const b: TByteDynArray): String;
begin
  SetLength(Result, Length(b));
  System.Move(b[0], Result[1], Length(b));
end;

function eth_bytesToHex(const b: TByteDynArray; HexPrefix: Boolean = True; Pad: Integer = 0): String;
begin
  Result := eth_bytesToStr(b);
  Result := eth_strToHex(Result, HexPrefix, Pad);
end;

function iif(const ACondition: boolean; const BranchTRUE, BranchFALSE: variant): variant;
begin
  if ACondition
    then result := BranchTRUE
    else result := BranchFALSE;
end;


{ TEth_BlockNumberHelper }

function TEth_BlockNumberHelper.ToString(BlockNumberCustom: Int64): String;
begin
  case Self of
    ethbnCustom:    Result := eth_intToHex(BlockNumberCustom);
    ethbnEearliest: Result := 'earliest';
    ethbnLatest:    Result := 'latest';
    ethbnPending:   Result := 'pending';
  end;
end;

{ TEth_getFilterChangesClass }

destructor TEth_getFilterChangesClass.Destroy;
var
  LresultItem: TEth_FilterChangeClass;
begin

 for LresultItem in FResult do
   LresultItem.free;

  inherited;
end;

{ TEth_BlockClass }

destructor TEth_BlockClass.Destroy;
var
  LtransactionsItem: TEth_TransactionClass;
begin

 for LtransactionsItem in FTransactions do
   LtransactionsItem.free;

  inherited;
end;

{ TEth_TransactionReceiptClass }

destructor TEth_TransactionReceiptClass.Destroy;
var
  LtransactionsItem: TEth_TransactionClass;
begin

 for LtransactionsItem in FLogs do
   LtransactionsItem.free;

  inherited;
end;

{ TAdmin_nodeinfo_ProtocolsClass }

constructor TAdmin_nodeinfo_ProtocolsClass.Create;
begin
  inherited;
  FEth := TAdmin_nodeinfo_Protocols_EthClass.Create();
end;

destructor TAdmin_nodeinfo_ProtocolsClass.Destroy;
begin
  FEth.free;
  inherited;
end;

{ TAdmin_nodeInfoClass }

constructor TAdmin_nodeInfoClass.Create;
begin
  inherited;
  FPorts := TAdmin_nodeinfo_PortsClass.Create();
  FProtocols := TAdmin_nodeinfo_ProtocolsClass.Create();
end;

destructor TAdmin_nodeInfoClass.Destroy;
begin
  FPorts.free;
  FProtocols.free;
  inherited;
end;

{ TAdmin_nodePeersClass }

constructor TAdmin_nodePeersClass.Create;
begin
  inherited;
  FNetwork := TAdmin_nodePeers_NetworkClass.Create();
  FProtocols := TAdmin_nodeinfo_ProtocolsClass.Create();
end;

destructor TAdmin_nodePeersClass.Destroy;
begin
  FNetwork.free;
  FProtocols.free;
  inherited;
end;


end.
