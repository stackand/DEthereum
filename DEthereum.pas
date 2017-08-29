unit DEthereum;

interface

uses
  System.Types, System.SysUtils, System.Classes, System.IOUtils,
  System.StrUtils, System.JSON, System.TypInfo,
  Generics.Collections, Rest.Json, IdHTTP,
  DEthereum.Types;

{
  Ethereum RPC API
  https://github.com/ethereum/wiki/wiki/JSON-RPC
  https://github.com/ethereum/go-ethereum/wiki/Management-APIs
}

const
  RPC_PARSE_ERROR      = -32700;

type
  TEthereumContract = class;

  TEthereum = class
  const
    OperationTimeout = 5 * MSecsPerSec;
  protected
    FRpcPort: Integer;
    FCoinPassword: String;
    FRpcAddress: String;
    FCoinAddress: String;
    FMethodResult: TJSONObject;
    FMethodCall: TJSONObject;
    FOnMethodCall: TNotifyEvent;
    FOnMethodResult: TNotifyEvent;
    FOnMethodError: TNotifyEvent;
    FMethodError: TEth_ErrorClass;
  public
    function RpcCallNew<T: class, constructor>(Method: String; Params: array of const; var CallResult: T): Boolean; overload;
    function RpcCallNew<T: class, constructor>(Method: String; var CallResult: T): Boolean; overload;
    function RpcCallNew1<T>(Method: String; Params: array of const; var CallResult: T): Boolean; overload;
    function RpcCallNew1<T>(Method: String; var CallResult: T): Boolean; overload;
    function RpcCall(Method: String; Params: array of const): Boolean; overload;
    function RpcCall(Method: String): Boolean; overload;
  published
    constructor Create;
    destructor Destroy; override;

    function web3_clientVersion(out CallResult: String): Boolean;
    function web3_sha3(Params: array of const; out CallResult: String): Boolean;
    function net_version(out CallResult: String): Boolean;
    function net_peerCount(out CallResult: Integer): Boolean;
    function net_listening(out CallResult: Boolean): Boolean;
    function eth_protocolVersion(out CallResult: String): Boolean;
    function eth_syncing(out CallResult: TEth_syncingClass): Boolean;
    function eth_coinbase(out CallResult: String): Boolean;
    function eth_mining(out CallResult: Boolean): Boolean;
    function eth_hashrate(out CallResult: Int64): Boolean;
    function eth_gasPrice(out CallResult: Integer): Boolean;
    function eth_accounts(out CallResult: TArray<String>): Boolean;
    function eth_blockNumber(out CallResult: Integer): Boolean;
    function eth_getBalance(const Address: String; const BlockNumber: TEth_BlockNumber; const BlockNumberCustom: Int64; out CallResult: Integer): Boolean;
    function eth_getStorageAt(const Address: String; const Position: Integer; const BlockNumber: TEth_BlockNumber; const BlockNumberCustom: Int64; out CallResult: String): Boolean;
    function eth_getTransactionCount(const Address: String; const BlockNumber: TEth_BlockNumber; const BlockNumberCustom: Int64; out CallResult: Integer): Boolean;
    function eth_getBlockTransactionCountByHash(const BlockHash: String; out CallResult: Integer): Boolean;
    function eth_getBlockTransactionCountByNumber(const BlockNumber: TEth_BlockNumber; const BlockNumberCustom: Int64; out CallResult: Integer): Boolean;
    function eth_getUncleCountByBlockHash(const BlockHash: String; out CallResult: Integer): Boolean;
    function eth_getUncleCountByBlockNumber(const BlockNumber: TEth_BlockNumber; const BlockNumberCustom: Int64; out CallResult: Integer): Boolean;
    function eth_getCode(const Address: String; const BlockNumber: TEth_BlockNumber; const BlockNumberCustom: Int64; out CallResult: String): Boolean;
    function eth_sign(const Address, Message: String; out CallResult: String): Boolean;
    function eth_sendTransaction(const from, &to: String; const gas, gasPrice, value: Integer; const data: String; out CallResult: String): Boolean;
    function eth_sendRawTransaction(const Data: String; out CallResult: String): Boolean;
    function eth_call(const from, &to: String; const gas, gasPrice, value: Int64; const data: String; const BlockNumber: TEth_BlockNumber; const BlockNumberCustom: Int64; out CallResult: String): Boolean;
    function eth_estimateGas(const from, &to: String; const gas, gasPrice, value: Integer; const data: String; out CallResult: Integer): Boolean;
    function eth_getBlockByHash(const BlockHash: String; const IncludeTransactions: Boolean; out CallResult: TEth_BlockClass): Boolean;
    function eth_getBlockByNumber(const Block: Integer; const IncludeTransactions: Boolean; out CallResult: TEth_BlockClass): Boolean;
    function eth_getTransactionByHash(const TransactionHash: String; out CallResult: TEth_TransactionClass): Boolean;
    function eth_getTransactionByBlockHashAndIndex(const BlockHash: String; const TransactionIndex: Integer; out CallResult: TEth_TransactionClass): Boolean;
    function eth_getTransactionByBlockNumberAndIndex(const BlockNumber: TEth_BlockNumber; const BlockNumberCustom, TransactionIndex: Int64; out CallResult: TEth_TransactionClass): Boolean;
    function eth_getTransactionReceipt(const TransactionHash: String; out CallResult: TEth_TransactionReceiptClass): Boolean;
    function eth_getUncleByBlockHashAndIndex(const BlockHash: String; const UncleIndex: Integer; out CallResult: TEth_BlockClass): Boolean;
    function eth_getUncleByBlockNumberAndIndex(const BlockNumber: TEth_BlockNumber; const BlockNumberCustom, UncleIndex: Int64; out CallResult: TEth_BlockClass): Boolean;
    function eth_getCompilers(out CallResult: TArray<String>): Boolean;
    function eth_compileLLL(const Source: String; out CallResult: String): Boolean;
    function eth_compileSolidity(const Source: String; out CallResult: String): Boolean;
    function eth_compileSerpent(const Source: String; out CallResult: String): Boolean;
    function eth_newFilter(const FromBlockNumber: TEth_BlockNumber; const FromBlockNumberCustom: Int64; const ToBlockNumber: TEth_BlockNumber; const ToBlockNumberCustom: Int64; const ContractAddress: String; const Topics: TArray<String>; out CallResult: Int64): Boolean;
    function eth_newBlockFilter(out CallResult: Integer): Boolean;
    function eth_newPendingTransactionFilter(out CallResult: Integer): Boolean;
    function eth_uninstallFilter(const FilterId: Integer; out CallResult: Boolean): Boolean;
    function eth_getFilterChanges(const FilterId: Int64; CallResult: TObjectList<TEth_FilterChangeClass>): Boolean;
    function eth_getFilterLogs(const FilterId: Int64; CallResult: TObjectList<TEth_FilterChangeClass>): Boolean;
    function eth_getLogs(const FromBlockNumber: TEth_BlockNumber; const FromBlockNumberCustom: Int64; const ToBlockNumber: TEth_BlockNumber; const ToBlockNumberCustom: Int64; const ContractAddress: String; const Topics: TArray<String>; CallResult: TObjectList<TEth_FilterChangeClass>): Boolean;
    function eth_getWork(out CurrentBlock, SeedHash, Target: String): Boolean;
    function eth_submitWork(out NonceFound, PowHash, MixDigest: String): Boolean;
    function eth_submitHashrate(const HashRate, ClientId: String; out CallResult: Boolean): Boolean;
    function db_putString(const Database, KeyName, KeyValue: String; out CallResult: Boolean): Boolean;
    function db_getString(const Database, KeyName: String; out KeyValue: String): Boolean;
    function db_putHex(const Database, KeyName, KeyValue: String; out CallResult: Boolean): Boolean;
    function db_getHex(const Database, KeyName: String; out KeyValue: String): Boolean;
    function shh_version(out ShhVersion: String): Boolean;
    function shh_newIdentity(out IdentityAddress: String): Boolean;
    function shh_hasIdentity(const IdentityAddress: String; out HasIdentity: Boolean): Boolean;
    function shh_newGroup(out GroupAddress: String): Boolean;
    function shh_addToGroup(const GroupAddress: String; out CallResult: Boolean): Boolean;
    function shh_post(const FromAddress, ToAddress: String; const Topics: TStringList; const Payload: String; const Priority, TTL: Integer; out CallResult: Boolean): Boolean;
{   TO-DO
    function shh_newFilter(): Boolean;
    function shh_uninstallFilter(): Boolean;
    function shh_getFilterChanges(): Boolean;
    function shh_getMessages(): Boolean;
}
    function personal_ecRecover(const message, signature: String; out CallResult: String): Boolean;
    function personal_importRawKey(const keydata, passphrase: String; out CallResult: String): Boolean;
    function personal_listAccounts(out CallResult: TArray<String>): Boolean;
    function personal_lockAccount(const address: String; out CallResult: Boolean): Boolean;
    function personal_newAccount(const password: String; out CallResult: String): Boolean;
    function personal_unlockAccount(const address, passphrase: String; duration: Int64; out CallResult: Boolean): Boolean;
    function personal_sendTransaction(const from, &to, passphrase: String; const value: Int64; out CallResult: String): Boolean;
    function personal_sign(const message, account, password: String; out CallResult: String): Boolean;
    function personal_signAndSendTransaction(const from, &to: String; const gas, gasPrice, value: Int64; const data, password: String; out CallResult: String): Boolean;

    function txpool_content(out CallResult: String): Boolean;
    function txpool_inspect(out CallResult: String): Boolean;
    function txpool_status(out CallResult: TTxpool_statusClass): Boolean;

    function miner_makeDAG(const BlockNumber: Int64; out CallResult: Boolean): Boolean;
    function miner_setExtra(const Extra: String; out CallResult: Boolean): Boolean;
    function miner_setGasPrice(const gasPrice: Int64; out CallResult: Boolean): Boolean;
    function miner_start(const ThreadsCount: Int64; out CallResult: Boolean): Boolean;
    function miner_startAutoDAG(out CallResult: Boolean): Boolean;
    function miner_stop(out CallResult: Boolean): Boolean;
    function miner_stopAutoDAG(out CallResult: Boolean): Boolean;

    function admin_addPeer(const Url: String; out CallResult: Boolean): Boolean;
    function admin_datadir(out CallResult: String): Boolean;
    function admin_nodeInfo(out CallResult: TAdmin_nodeInfoClass): Boolean;
    function admin_peers(out CallResult: TAdmin_nodePeersClass): Boolean;
    function admin_setSolc(const Path: String; out CallResult: String): Boolean;
    function admin_startRPC(const host: String; const port: Integer; const cors, apis: String; out CallResult: Boolean): Boolean;
    function admin_startWS(const host: String; const port: Integer; const cors, apis: String; out CallResult: Boolean): Boolean;
    function admin_stopRPC(out CallResult: Boolean): Boolean;
    function admin_stopWS(out CallResult: Boolean): Boolean;

{   TO-DO
    function debug_backtraceAt(out CallResult: Boolean): Boolean;
    function debug_blockProfile(out CallResult: Boolean): Boolean;
    function debug_cpuProfile(out CallResult: Boolean): Boolean;
    function debug_dumpBlock(out CallResult: Boolean): Boolean;
    function debug_gcStats(out CallResult: Boolean): Boolean;
    function debug_getBlockRlp(out CallResult: Boolean): Boolean;
    function debug_goTrace(out CallResult: Boolean): Boolean;
    function debug_memStats(out CallResult: Boolean): Boolean;
    function debug_seedHashsign(out CallResult: Boolean): Boolean;
    function debug_setBlockProfileRate(out CallResult: Boolean): Boolean;
    function debug_setHead(out CallResult: Boolean): Boolean;
    function debug_stacks(out CallResult: Boolean): Boolean;
    function debug_startCPUProfile(out CallResult: Boolean): Boolean;
    function debug_startGoTrace(out CallResult: Boolean): Boolean;
    function debug_stopCPUProfile(out CallResult: Boolean): Boolean;
    function debug_stopGoTrace(out CallResult: Boolean): Boolean;
    function debug_traceBlock(out CallResult: Boolean): Boolean;
    function debug_traceBlockByNumber(out CallResult: Boolean): Boolean;
    function debug_traceBlockByHash(out CallResult: Boolean): Boolean;
    function debug_traceBlockFromFile(out CallResult: Boolean): Boolean;
    function debug_traceTransaction(out CallResult: Boolean): Boolean;
    function debug_verbosity(out CallResult: Boolean): Boolean;
    function debug_vmodule(out CallResult: Boolean): Boolean;
    function debug_writeBlockProfile(out CallResult: Boolean): Boolean;
    function debug_writeMemProfile(out CallResult: Boolean): Boolean;
}
    property RpcAddress: String read FRpcAddress write FRpcAddress;
    property RpcPort: Integer read FRpcPort write FRpcPort;
    property CoinAddress: String read FCoinAddress write FCoinAddress;
    property CoinPassword: String read FCoinPassword write FCoinPassword;
    property MethodCall: TJSONObject read FMethodCall;
    property MethodResult: TJSONObject read FMethodResult;
    property MethodError: TEth_ErrorClass read FMethodError;
    property OnMethodError: TNotifyEvent read FOnMethodError write FOnMethodError;
    property OnMethodCall: TNotifyEvent read FOnMethodCall write FOnMethodCall;
    property OnMethodResult: TNotifyEvent read FOnMethodResult write FOnMethodResult;
  end;


  TEthereumContractParameter = class
  private
    FName: String;
    FType: String;
    FValue: String;
    FIndexed: Boolean;
    function GetAsInteger: Integer;
    function GetAsString: String;
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsString(const Value: String);
    function GetAsInt64: Int64;
    procedure SetAsInt64(const Value: Int64);
    function GetAsBytes: TByteDynArray;
    procedure SetAsBytes(const Value: TByteDynArray);
    function GetAsUInt32: UInt32;
    function GetAsUInt64: UInt64;
    procedure SetAsUInt64(const Value: UInt64);
    procedure SetAsUInt32(const Value: UInt32);
    function GetAsByte: Byte;
    procedure SetAsByte(const Value: Byte);
  public
    property Name: String read FName write FName;
    property &Type: String read FType write FType;
    property Indexed: Boolean read FIndexed write FIndexed;
    property Value: String read FValue write FValue;
    property AsString: String read GetAsString write SetAsString;
    property AsByte: Byte read GetAsByte write SetAsByte;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsUInt32: UInt32 read GetAsUInt32 write SetAsUInt32;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsUInt64: UInt64 read GetAsUInt64 write SetAsUInt64;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsBytes: TByteDynArray read GetAsBytes write SetAsBytes;
//    property AsInt256: TBigInt read GetAsInt256;
  end;


  TEthereumContractMethod = class
  type
    TNameTypeConvertor = procedure(var EthName, EthType: String) of object;
  private
    FMethodName: String;
    FMethodType: String;
    FMethodConstant: Boolean;
    FMethodPayable: Boolean;
    FMethodHash: String;
    FInputs: TObjectList<TEthereumContractParameter>;
    FOutputs: TObjectList<TEthereumContractParameter>;
  public
    constructor Create;
    destructor Destroy; override;
    function DelphiMethodSignature(ContractClassName: String): String;
    function EthereumMethodSignature: String;
    property MethodName: String read FMethodName write FMethodName;
    property MethodType: String read FMethodType write FMethodType;
    property MethodPayable: Boolean read FMethodPayable write FMethodPayable;
    property MethodConstant: Boolean read FMethodConstant write FMethodConstant;
    property MethodHash: String read FMethodHash write FMethodHash;
    property Inputs: TObjectList<TEthereumContractParameter> read FInputs;
    property Outputs: TObjectList<TEthereumContractParameter> read FOutputs;
  end;

  TEthereumContractEvent = class
  private
    FEventName: String;
    FEventType: String;
    FEventHash: String;
    FEventAnonymous: Boolean;
    FEvents: TObjectList<TEth_FilterChangeClass>;
    FParameters: TObjectList<TEthereumContractParameter>;
  public
    constructor Create;
    destructor Destroy; override;
    function DelphiEventSignature(ContractClassName: String): String;
    function EthereumEventSignature: String;
    property EventName: String read FEventName write FEventName;
    property EventType: String read FEventType write FEventType;
    property EventHash: String read FEventHash write FEventHash;
    property EventAnonymous: Boolean read FEventAnonymous write FEventAnonymous;
    property Events: TObjectList<TEth_FilterChangeClass> read FEvents;
    property Parameters: TObjectList<TEthereumContractParameter> read FParameters;
  end;

  TEthereumContract = class(TEthereum)
  type
    TNameTypeConvertor = procedure(var EthName, EthType: String) of object;
  private
    FContractAddress: String;
    FMethods: TObjectList<TEthereumContractMethod>;
    FEvents: TObjectList<TEthereumContractEvent>;
  protected
    function GetEvent(EventName: String): TEthereumContractEvent;
    function GetMethod(MethodName: String): TEthereumContractMethod;
    function ErrorNotFound(Source: String): Boolean;
    function ErrorBadCallCode(Source: String): Boolean;
    function ErrorTypeCast(Source: String; Param: TEthereumContractParameter; PassedType: String): Boolean;
    function ErrorLengthMismatch(Source: String; Param: TEthereumContractParameter): Boolean;
    function ErrorParamConvert(Source: String; E: Exception): Boolean;
    function ErrorNotEnoughParameters(Source: String): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    class procedure DelphiSimpleConvertor(var EthName, EthType: String);
    class procedure DelphiForDynArrayConvertor(var EthName, EthType: String);
    class function NamesTypes(Parameters: TObjectList<TEthereumContractParameter>;
      Prefix, ParameterDelimiter, TypeSeparator: String;
      IncludeName, IncludeType: Boolean;
      Convertor: TNameTypeConvertor): String;
    class function DelphiParametersAssigner(Parameters: TObjectList<TEthereumContractParameter>; ParamNamePrefix: String): String;
    function ParseABI(const JSONString: String): Boolean;
    function BuildABI(out JSONString: String; const Pretty: Boolean = False; const IncludeValue: Boolean = False): Boolean;
    function BuildDelphiSource(ContractName: String; Output: TStrings): Boolean;
    function BuildCallCode(Method: TEthereumContractMethod; Params: array of const; out Code: String): Boolean; overload;
    function BuildCallCode(Method: TEthereumContractMethod; out Code: String): Boolean; overload;
    function ParseCallCode(Source, Code: String; Destination: TObjectList<TEthereumContractParameter>): Boolean;
    function GetEventHash(Event: TEthereumContractEvent): Boolean;
    function GetMethodHash(Method: TEthereumContractMethod): Boolean;
    property ContractAddress: String read FContractAddress write FContractAddress;
    property Methods: TObjectList<TEthereumContractMethod> read FMethods;
    property Method[MethodName: String]: TEthereumContractMethod read GetMethod;
    property Events: TObjectList<TEthereumContractEvent> read FEvents;
    property Event[EventName: String]: TEthereumContractEvent read GetEvent;
  end;

implementation

{ TEthereum }

constructor TEthereum.Create;
begin
  inherited;
  FRpcPort := 8545;
  FRpcAddress := 'localhost';
end;

function TEthereum.db_getHex(const Database, KeyName: String;
  out KeyValue: String): Boolean;
begin
  Result := RpcCallNew1<String>('db_getHex', [Database, KeyName], KeyValue);
  KeyValue := eth_hexToStr(KeyValue);
end;

function TEthereum.db_getString(const Database, KeyName: String;
  out KeyValue: String): Boolean;
begin
  Result := RpcCallNew1<String>('db_getString', [Database, KeyName], KeyValue);
end;

function TEthereum.db_putHex(const Database, KeyName, KeyValue: String;
  out CallResult: Boolean): Boolean;
begin
  Result := RpcCallNew1<Boolean>('db_putHex', [Database, KeyName, eth_strToHex(KeyValue)], CallResult);
end;

function TEthereum.db_putString(const Database, KeyName, KeyValue: String;
  out CallResult: Boolean): Boolean;
begin
  Result := RpcCallNew1<Boolean>('db_putString', [Database, KeyName, KeyValue], CallResult);
end;

destructor TEthereum.Destroy;
begin
  FreeAndNil(FMethodCall);
  FreeAndNil(FMethodResult);
  FreeAndNil(FMethodError);

  inherited;
end;

function TEthereum.eth_accounts(out CallResult: TArray<String>): Boolean;
var
  Accounts: TEth_StringArrayResultClass;
begin
  Result := RpcCallNew<TEth_StringArrayResultClass>('eth_accounts', Accounts);
  if Assigned(Accounts) then
    begin
      CallResult := Accounts.result;
      Accounts.Free;
    end;
end;

function TEthereum.eth_blockNumber(out CallResult: Integer): Boolean;
var
  s: String;
begin
  Result := RpcCallNew1<String>('eth_blockNumber', s);
  if Result then
    CallResult := eth_hexToInt(s);
end;

function TEthereum.eth_call(const from, &to: String;
  const gas, gasPrice, value: Int64; const data: String;
  const BlockNumber: TEth_BlockNumber; const BlockNumberCustom: Int64;
  out CallResult: String): Boolean;
var
  ParamObject: TJSONObject;
begin
  ParamObject := TJSONObject.Create;
  try

    ParamObject.AddPair(TJSONPair.Create('to', &to));
    if from <> '' then ParamObject.AddPair(TJSONPair.Create('from', from));
    if gas <> 0 then ParamObject.AddPair(TJSONPair.Create('gas', eth_intToHex(gas)));
    if gasPrice <> 0 then ParamObject.AddPair(TJSONPair.Create('gasPrice', eth_intToHex(gasPrice)));
    if value <> 0 then ParamObject.AddPair(TJSONPair.Create('value', eth_intToHex(value)));
    if data <> '' then ParamObject.AddPair(TJSONPair.Create('data', data));

    Result := RpcCallNew1<String>('eth_call', [ParamObject, BlockNumber.ToString(BlockNumberCustom)], CallResult);
  finally
    ParamObject.Free;
  end;
end;

function TEthereum.eth_coinbase(out CallResult: String): Boolean;
begin
  Result := RpcCallNew1<String>('eth_coinbase', CallResult);
end;

function TEthereum.eth_compileLLL(const Source: String;
  out CallResult: String): Boolean;
begin
  Result := RpcCallNew1<String>('eth_compileLLL', [Source], CallResult);
end;

function TEthereum.eth_compileSerpent(const Source: String;
  out CallResult: String): Boolean;
begin
  Result := RpcCallNew1<String>('eth_compileSerpent', [Source], CallResult);
end;

function TEthereum.eth_compileSolidity(const Source: String;
  out CallResult: String): Boolean;
var
  JSV: TJSONValue;
begin
  Result := RpcCall('eth_compileSolidity', [Source]);

  if Result then
    begin
      JSV := FMethodResult.GetValue('result');
      CallResult := JSV.ToJSON;
    end;
end;

function TEthereum.eth_estimateGas(const from, &to: String; const gas, gasPrice,
  value: Integer; const data: String; out CallResult: Integer): Boolean;
var
  s: String;
  ParamObject: TJSONObject;
begin
  ParamObject := TJSONObject.Create;
  try

    if from <> '' then ParamObject.AddPair(TJSONPair.Create('from', from));
    if &to <> '' then ParamObject.AddPair(TJSONPair.Create('to', &to));
    if gas <> 0 then ParamObject.AddPair(TJSONPair.Create('gas', eth_intToHex(gas)));
    if gasPrice <> 0 then ParamObject.AddPair(TJSONPair.Create('gas', eth_intToHex(gasPrice)));
    if value <> 0 then ParamObject.AddPair(TJSONPair.Create('value', eth_intToHex(value)));
    if data <> '' then ParamObject.AddPair(TJSONPair.Create('data', data));

    Result := RpcCallNew1<String>('eth_estimateGas', [ParamObject], s);
  finally
    ParamObject.Free;
  end;

  if Result then
    CallResult := eth_hexToInt(s);
end;

function TEthereum.eth_gasPrice(out CallResult: Integer): Boolean;
var
  s: String;
begin
  Result := RpcCallNew1<String>('eth_gasPrice', s);
  if Result then
    CallResult := eth_hexToInt(s);
end;

function TEthereum.eth_getBalance(const Address: String;
  const BlockNumber: TEth_BlockNumber; const BlockNumberCustom: Int64;
  out CallResult: Integer): Boolean;
var
  s: String;
begin
  Result := RpcCallNew1<String>('eth_getBalance', [Address, BlockNumber.ToString(BlockNumberCustom) ], s);
  if Result then
    CallResult := eth_hexToInt(s);
end;

function TEthereum.eth_getBlockByHash(const BlockHash: String; const IncludeTransactions: Boolean;
  out CallResult: TEth_BlockClass): Boolean;
begin
  Result := RpcCallNew<TEth_BlockClass>('eth_getBlockByHash', [BlockHash, IncludeTransactions], CallResult);
end;

function TEthereum.eth_getBlockByNumber(const Block: Integer; const IncludeTransactions: Boolean;
  out CallResult: TEth_BlockClass): Boolean;
begin
  Result := RpcCallNew<TEth_BlockClass>('eth_getBlockByNumber', [eth_intToHex(Block), IncludeTransactions], CallResult);
end;

function TEthereum.eth_getBlockTransactionCountByHash(const BlockHash: String;
  out CallResult: Integer): Boolean;
var
  s: String;
begin
  Result := RpcCallNew1<String>('eth_getBlockTransactionCountByHash', [BlockHash], s);
  if Result then
    CallResult := eth_hexToInt(s);
end;

function TEthereum.eth_getBlockTransactionCountByNumber(
  const BlockNumber: TEth_BlockNumber; const BlockNumberCustom: Int64;
  out CallResult: Integer): Boolean;
var
  s: String;
begin
  Result := RpcCallNew1<String>('eth_getBlockTransactionCountByNumber', [BlockNumber.ToString(BlockNumberCustom)], s);
  if Result then
    CallResult := eth_hexToInt(s);
end;

function TEthereum.eth_getCode(const Address: String;
  const BlockNumber: TEth_BlockNumber; const BlockNumberCustom: Int64;
  out CallResult: String): Boolean;
begin
  Result := RpcCallNew1<String>('eth_getCode', [Address, BlockNumber.ToString(BlockNumberCustom)], CallResult);
end;

function TEthereum.eth_getCompilers(out CallResult: TArray<String>): Boolean;
var
  Compilers: TEth_StringArrayResultClass;
begin
  Result := RpcCallNew<TEth_StringArrayResultClass>('eth_getCompilers', Compilers);
  if Assigned(Compilers) then
    begin
      CallResult := Compilers.result;
      Compilers.Free;
    end;
end;

function TEthereum.eth_getFilterChanges(const FilterId: Int64;
  CallResult: TObjectList<TEth_FilterChangeClass>): Boolean;
var
  Change: TEth_FilterChangeClass;
  Changes: TEth_getFilterChangesClass;
begin
  Result := RpcCallNew<TEth_getFilterChangesClass>('eth_getFilterChanges', [eth_intToHex(FilterId)], Changes);

  if Result then
    begin
      for Change in Changes.result do
        CallResult.Add(Change);
      Changes.result := [];
      Changes.Free;
    end;
end;

function TEthereum.eth_getFilterLogs(const FilterId: Int64;
  CallResult: TObjectList<TEth_FilterChangeClass>): Boolean;
var
  Change: TEth_FilterChangeClass;
  Changes: TEth_getFilterChangesClass;
begin
  Result := RpcCallNew<TEth_getFilterChangesClass>('eth_getFilterLogs', [eth_intToHex(FilterId)], Changes);
  if Result then
    begin
      for Change in Changes.result do
        CallResult.Add(Change);
      Changes.result := [];
      Changes.Free;
    end;
end;

function TEthereum.eth_getLogs(const FromBlockNumber: TEth_BlockNumber;
  const FromBlockNumberCustom: Int64; const ToBlockNumber: TEth_BlockNumber;
  const ToBlockNumberCustom: Int64; const ContractAddress: String;
  const Topics: TArray<String>;
  CallResult: TObjectList<TEth_FilterChangeClass>): Boolean;
var
  s: String;
  jsa: TJSONArray;
  ParamObject: TJSONObject;
  Change: TEth_FilterChangeClass;
  Changes: TEth_getFilterChangesClass;
begin
  ParamObject := TJSONObject.Create;
  try
    ParamObject.AddPair(TJSONPair.Create('fromBlock', FromBlockNumber.ToString(FromBlockNumberCustom)));
    ParamObject.AddPair(TJSONPair.Create('toBlock', ToBlockNumber.ToString(ToBlockNumberCustom)));
    ParamObject.AddPair(TJSONPair.Create('address', ContractAddress));

    jsa := TJSONArray.Create;
    for s in Topics do jsa.Add(s);
    ParamObject.AddPair(TJSONPair.Create('topics', jsa));

    Result := RpcCallNew<TEth_getFilterChangesClass>('eth_getLogs', [ParamObject], Changes);

    if Result then
      begin
        for Change in Changes.result do
          CallResult.Add(Change);
        Changes.result := [];
        Changes.Free;
      end;
  finally
    ParamObject.Free;
  end;
end;

function TEthereum.eth_getStorageAt(const Address: String;
  const Position: Integer; const BlockNumber: TEth_BlockNumber;
  const BlockNumberCustom: Int64; out CallResult: String): Boolean;
var
  s: String;
begin
  Result := RpcCallNew1<String>('eth_getStorageAt', [Address, Position, BlockNumber.ToString(BlockNumberCustom)], CallResult);
end;

function TEthereum.eth_getTransactionByBlockHashAndIndex(
  const BlockHash: String; const TransactionIndex: Integer;
  out CallResult: TEth_TransactionClass): Boolean;
begin
  Result := RpcCallNew<TEth_TransactionClass>('eth_getTransactionByBlockHashAndIndex', [BlockHash, TransactionIndex], CallResult);
end;

function TEthereum.eth_getTransactionByBlockNumberAndIndex(
  const BlockNumber: TEth_BlockNumber; const BlockNumberCustom,
  TransactionIndex: Int64; out CallResult: TEth_TransactionClass): Boolean;
begin
  Result := RpcCallNew<TEth_TransactionClass>('eth_getTransactionByBlockNumberAndIndex', [BlockNumber.ToString(BlockNumberCustom), eth_intToHex(TransactionIndex)], CallResult);
end;

function TEthereum.eth_getTransactionByHash(const TransactionHash: String;
  out CallResult: TEth_TransactionClass): Boolean;
begin
  Result := RpcCallNew<TEth_TransactionClass>('eth_getTransactionByHash', [TransactionHash], CallResult);
end;

function TEthereum.eth_getTransactionCount(const Address: String;
  const BlockNumber: TEth_BlockNumber; const BlockNumberCustom: Int64;
  out CallResult: Integer): Boolean;
var
  s: String;
begin
  Result := RpcCallNew1<String>('eth_getTransactionCount', [Address, BlockNumber.ToString(BlockNumberCustom)], s);
  if Result then
    CallResult := eth_hexToInt(s);
end;

function TEthereum.eth_getTransactionReceipt(const TransactionHash: String;
  out CallResult: TEth_TransactionReceiptClass): Boolean;
begin
  Result := RpcCallNew<TEth_TransactionReceiptClass>('eth_getTransactionReceipt', [TransactionHash], CallResult);
end;

function TEthereum.eth_getUncleByBlockHashAndIndex(const BlockHash: String;
  const UncleIndex: Integer; out CallResult: TEth_BlockClass): Boolean;
begin
  Result := RpcCallNew<TEth_BlockClass>('eth_getUncleByBlockHashAndIndex', [BlockHash, eth_intToHex(UncleIndex)], CallResult);
end;

function TEthereum.eth_getUncleByBlockNumberAndIndex(
  const BlockNumber: TEth_BlockNumber; const BlockNumberCustom,
  UncleIndex: Int64; out CallResult: TEth_BlockClass): Boolean;
begin
  Result := RpcCallNew<TEth_BlockClass>('eth_getUncleByBlockNumberAndIndex', [BlockNumber.ToString(BlockNumberCustom), eth_intToHex(UncleIndex)], CallResult);
end;

function TEthereum.eth_getUncleCountByBlockHash(const BlockHash: String;
  out CallResult: Integer): Boolean;
var
  s: String;
begin
  Result := RpcCallNew1<String>('eth_getUncleCountByBlockHash', [BlockHash], s);
  if Result then
    CallResult := eth_hexToInt(s);
end;

function TEthereum.eth_getUncleCountByBlockNumber(const BlockNumber: TEth_BlockNumber;
  const BlockNumberCustom: Int64;
  out CallResult: Integer): Boolean;
var
  s: String;
begin
  Result := RpcCallNew1<String>('eth_getUncleCountByBlockNumber', [BlockNumber.ToString(BlockNumberCustom)], s);
  if Result then
    CallResult := eth_hexToInt(s);
end;

function TEthereum.eth_getWork(out CurrentBlock, SeedHash,
  Target: String): Boolean;
var
  StringArray: TEth_StringArrayResultClass;
begin
  Result := RpcCallNew<TEth_StringArrayResultClass>('eth_getWork', StringArray);
  if Assigned(StringArray) then
    begin
      CurrentBlock := StringArray.result[0];
      SeedHash := StringArray.result[1];
      Target := StringArray.result[2];
      StringArray.Free;
    end;
end;

function TEthereum.eth_hashrate(out CallResult: Int64): Boolean;
var
  s: String;
begin
  Result := RpcCallNew1<String>('eth_hashrate', s);
  if Result then
    CallResult := eth_hexToInt(s);
end;

function TEthereum.eth_mining(out CallResult: Boolean): Boolean;
begin
  Result := RpcCallNew1<Boolean>('eth_mining', CallResult);
end;

function TEthereum.eth_newBlockFilter(out CallResult: Integer): Boolean;
var
  s: String;
begin
  Result := RpcCallNew1<String>('eth_newBlockFilter', s);
  if Result then
    CallResult := eth_hexToInt(s);
end;

function TEthereum.eth_newFilter(const FromBlockNumber: TEth_BlockNumber;
  const FromBlockNumberCustom: Int64; const ToBlockNumber: TEth_BlockNumber;
  const ToBlockNumberCustom: Int64; const ContractAddress: String;
  const Topics: TArray<String>; out CallResult: Int64): Boolean;
var
  s: String;
  jsa: TJSONArray;
  ParamObject: TJSONObject;
begin
  ParamObject := TJSONObject.Create;
  try

    ParamObject.AddPair(TJSONPair.Create('fromBlock', FromBlockNumber.ToString(FromBlockNumberCustom)));
    ParamObject.AddPair(TJSONPair.Create('toBlock', ToBlockNumber.ToString(ToBlockNumberCustom)));
    ParamObject.AddPair(TJSONPair.Create('address', ContractAddress));

    jsa := TJSONArray.Create;
    for s in Topics do jsa.Add(s);
    ParamObject.AddPair(TJSONPair.Create('topics', jsa));

    Result := RpcCallNew1<String>('eth_newFilter', [ParamObject], s);
    if Result then
      CallResult := eth_hexToInt(s);
  finally
    ParamObject.Free;
  end;
end;

function TEthereum.eth_newPendingTransactionFilter(
  out CallResult: Integer): Boolean;
var
  s: String;
begin
  Result := RpcCallNew1<String>('eth_newPendingTransactionFilter', s);
  if Result then
    CallResult := eth_hexToInt(s);
end;

function TEthereum.eth_protocolVersion(out CallResult: String): Boolean;
begin
  Result := RpcCallNew1<String>('eth_protocolVersion', CallResult);
end;

function TEthereum.eth_sendRawTransaction(const Data: String;
  out CallResult: String): Boolean;
begin
  Result := RpcCallNew1<String>('eth_sendRawTransaction', [Data], CallResult);
end;

function TEthereum.eth_sendTransaction(const from, &to: String; const gas, gasPrice, value: Integer;
  const data: String; out CallResult: String): Boolean;
begin
  Result := RpcCallNew1<String>('eth_sendTransaction', [from, &to, eth_intToHex(gas), eth_intToHex(gasPrice), eth_intToHex(value), data], CallResult);
end;

function TEthereum.eth_sign(const Address, Message: String;
  out CallResult: String): Boolean;
begin
  Result := RpcCallNew1<String>('eth_sign', [Address, eth_strToHex(Message)], CallResult);
end;

function TEthereum.eth_submitHashrate(const HashRate, ClientId: String;
  out CallResult: Boolean): Boolean;
begin
  Result := RpcCallNew1<Boolean>('eth_submitHashrate', [HashRate, ClientId], CallResult);
end;

function TEthereum.eth_submitWork(out NonceFound, PowHash,
  MixDigest: String): Boolean;
var
  StringArray: TEth_StringArrayResultClass;
begin
  Result := RpcCallNew<TEth_StringArrayResultClass>('eth_submitWork', StringArray);
  if Assigned(StringArray) then
    begin
      NonceFound := StringArray.result[0];
      PowHash := StringArray.result[1];
      MixDigest := StringArray.result[2];
      StringArray.Free;
    end;
end;

function TEthereum.eth_uninstallFilter(const FilterId: Integer;
  out CallResult: Boolean): Boolean;
begin
  Result := RpcCallNew1<Boolean>('eth_uninstallFilter', [eth_intToHex(FilterId)], CallResult);
end;

function TEthereum.miner_makeDAG(const BlockNumber: Int64; out CallResult: Boolean): Boolean;
begin
  Result := RpcCallNew1<Boolean>('miner_makeDAG', [eth_intToHex(BlockNumber)], CallResult);
end;

function TEthereum.miner_setExtra(const Extra: String;
  out CallResult: Boolean): Boolean;
begin
  Result := RpcCallNew1<Boolean>('miner_setExtra', [eth_strToHex(Extra)], CallResult);
end;

function TEthereum.miner_setGasPrice(const gasPrice: Int64;
  out CallResult: Boolean): Boolean;
begin
  Result := RpcCallNew1<Boolean>('miner_setGasPrice', [eth_intToHex(gasPrice)], CallResult);
end;

function TEthereum.miner_start(const ThreadsCount: Int64;
  out CallResult: Boolean): Boolean;
begin
  Result := RpcCallNew1<Boolean>('miner_start', [ThreadsCount], CallResult);
end;

function TEthereum.miner_startAutoDAG(out CallResult: Boolean): Boolean;
begin
  Result := RpcCallNew1<Boolean>('miner_startAutoDAG', CallResult);
end;

function TEthereum.miner_stop(out CallResult: Boolean): Boolean;
begin
  Result := RpcCallNew1<Boolean>('miner_stop', CallResult);
end;

function TEthereum.miner_stopAutoDAG(out CallResult: Boolean): Boolean;
begin
  Result := RpcCallNew1<Boolean>('miner_stopAutoDAG', CallResult);
end;

function TEthereum.RpcCall(Method: String; Params: array of const): Boolean;
var
  i, j: Integer;
  Http: TIdHttp;
  JSA: TJSONArray;
  JSV: TJSONValue;
  s: String;
  StringList: TStringList;
  RequestStream, ResponseStream: TStringStream;
begin
  Http := TIdHttp.Create(nil);
  RequestStream := TStringStream.Create;
  ResponseStream := TStringStream.Create;

  Result := False;

  FreeAndNil(FMethodCall);
  FreeAndNil(FMethodResult);
  FreeAndNil(FMethodError);

  try
    Http.ConnectTimeout := OperationTimeout;
    Http.ReadTimeout := OperationTimeout;

    Http.Request.BasicAuthentication := True;
    Http.Request.Username := FCoinAddress;
    Http.Request.Password := FCoinPassword;

    try
      JSA := TJSONArray.Create;

      for i := Low(Params) to High(Params) do
        case TVarRec(Params[i]).VType of
          vtBoolean:        JSA.AddElement(TJSONBool.Create(TVarRec(Params[i]).VBoolean));
          vtInteger:        JSA.AddElement(TJSONNumber.Create(TVarRec(Params[i]).VInteger));
          vtInt64:          JSA.AddElement(TJSONNumber.Create(TVarRec(Params[i]).VInt64^));
          vtExtended:       JSA.AddElement(TJSONNumber.Create(Extended(TVarRec(Params[i]).VExtended^)));
          vtAnsiString:     JSA.AddElement(TJSONString.Create(String(AnsiString(TVarRec(Params[i]).VAnsiString))));
          vtUnicodeString:  JSA.AddElement(TJSONString.Create(String(UnicodeString(TVarRec(Params[i]).VUnicodeString))));
          vtObject:
            if TVarRec(Params[i]).VObject is TStringList then
              begin
                StringList := TVarRec(Params[i]).VObject as TStringList;
                for s in StringList do
                  JSA.Add(s);
              end else
            if TVarRec(Params[i]).VObject is TJSONObject then
              JSA.AddElement((TVarRec(Params[i]).VObject as TJSONObject).Clone as TJSONObject);
        end;

      FMethodCall := TJSONObject.Create;
      FMethodCall.AddPair('id', TJSONNumber.Create(Integer(JSA)));
      FMethodCall.AddPair('jsonrpc', '2.0');
      FMethodCall.AddPair('method', Method);
      FMethodCall.AddPair('params', JSA);

      if Assigned(FOnMethodCall) then FOnMethodCall(Self);

      RequestStream.WriteString(FMethodCall.ToJSON);

      Http.Post(Format('http://%s:%d', [FRpcAddress, FRpcPort]), RequestStream, ResponseStream);

      if Http.ResponseCode = 200 then
        begin
          JSV := TJSONObject.ParseJSONValue(ResponseStream.DataString);
          if JSV is TJSONObject then
            begin
              FMethodResult := JSV as TJSONObject;
              if Assigned(FOnMethodResult) then FOnMethodResult(Self);

              JSV := FMethodResult.GetValue('error');
              if JSV is TJSONObject then
                FMethodError := Rest.Json.TJson.JsonToObject<TEth_ErrorClass>(JSV as TJSONObject);
            end
          else
            begin
              FreeAndNil(JSV);

              FMethodError := TEth_ErrorClass.Create;
              FMethodError.code := RPC_PARSE_ERROR;
              FMethodError.message := ResponseStream.DataString;
            end;

        end else
        begin
          FMethodError := TEth_ErrorClass.Create;
          FMethodError.code := Http.ResponseCode;
          FMethodError.message := ResponseStream.DataString;
        end;
    except
      on E: Exception do
        begin
          FMethodError := TEth_ErrorClass.Create;
          FMethodError.code := 0;
          FMethodError.message := E.Message;
        end;
    end;
  finally
    Http.Free;
    RequestStream.Free;
    ResponseStream.Free;

    Result := not Assigned(FMethodError);

    if not Result and Assigned(FOnMethodError) then FOnMethodError(Self);
  end;
end;

function TEthereum.RpcCall(Method: String): Boolean;
begin
  Result := RpcCall(Method, []);
end;

function TEthereum.RpcCallNew1<T>(Method: String;
  var CallResult: T): Boolean;
begin
  Result := RpcCallNew1<T>(Method, [], CallResult);
end;

function TEthereum.RpcCallNew1<T>(Method: String; Params: array of const;
  var CallResult: T): Boolean;
var
  JSV: TJSONValue;
begin
  Result := RpcCall(Method, Params);

  if Result then
    begin
      JSV := FMethodResult.GetValue('result');
      Result := JSV.TryGetValue<T>(CallResult);
    end;
end;

function TEthereum.RpcCallNew<T>(Method: String; var CallResult: T): Boolean;
begin
  Result := RpcCallNew<T>(Method, [], CallResult);
end;

function TEthereum.shh_addToGroup(const GroupAddress: String;
  out CallResult: Boolean): Boolean;
begin
  Result := RpcCallNew1<Boolean>('shh_addToGroup', [GroupAddress], CallResult);
end;

function TEthereum.shh_hasIdentity(const IdentityAddress: String;
  out HasIdentity: Boolean): Boolean;
begin
  Result := RpcCallNew1<Boolean>('shh_hasIdentity', [IdentityAddress], HasIdentity);
end;

function TEthereum.shh_newGroup(out GroupAddress: String): Boolean;
begin
  Result := RpcCallNew1<String>('shh_newGroup', GroupAddress);
end;

function TEthereum.shh_newIdentity(out IdentityAddress: String): Boolean;
begin
  Result := RpcCallNew1<String>('shh_newIdentity', IdentityAddress);
end;

function TEthereum.shh_post(const FromAddress, ToAddress: String;
  const Topics: TStringList; const Payload: String; const Priority,
  TTL: Integer; out CallResult: Boolean): Boolean;
var
  s: String;
  jsa: TJSONArray;
  ParamObject: TJSONObject;
begin
  ParamObject := TJSONObject.Create;
  try
    if FromAddress = ''
      then ParamObject.AddPair(TJSONPair.Create('from', CoinAddress))
      else ParamObject.AddPair(TJSONPair.Create('from', FromAddress));
    ParamObject.AddPair(TJSONPair.Create('to', ToAddress));

    jsa := TJSONArray.Create;
    for s in Topics do jsa.Add(s);
    ParamObject.AddPair(TJSONPair.Create('topics', jsa));

    ParamObject.AddPair(TJSONPair.Create('payload', eth_strToHex(Payload)));
    ParamObject.AddPair(TJSONPair.Create('priority', eth_intToHex(Priority)));
    ParamObject.AddPair(TJSONPair.Create('ttl', eth_intToHex(TTL)));

    Result := RpcCallNew1<Boolean>('shh_post', [ParamObject], CallResult);
  finally
    ParamObject.Free;
  end;
end;

function TEthereum.shh_version(out ShhVersion: String): Boolean;
begin
  Result := RpcCallNew1<String>('shh_version', ShhVersion);
end;

function TEthereum.txpool_content(out CallResult: String): Boolean;
var
  JSV: TJSONValue;
begin
  Result := RpcCall('txpool_content');

  if Result then
    begin
      JSV := FMethodResult.GetValue('result');
      CallResult := JSV.ToJSON;
    end;
end;

function TEthereum.txpool_inspect(out CallResult: String): Boolean;
var
  JSV: TJSONValue;
begin
  Result := RpcCall('txpool_inspect');

  if Result then
    begin
      JSV := FMethodResult.GetValue('result');
      CallResult := JSV.ToJSON;
    end;
end;

function TEthereum.txpool_status(out CallResult: TTxpool_statusClass): Boolean;
begin
  Result := RpcCallNew<TTxpool_statusClass>('txpool_status', CallResult);
end;

function TEthereum.eth_syncing(out CallResult: TEth_syncingClass): Boolean;
begin
  Result := RpcCallNew<TEth_syncingClass>('eth_syncing', CallResult);
end;

function TEthereum.web3_clientVersion(out CallResult: String): Boolean;
begin
  Result := RpcCallNew1<String>('web3_clientVersion', CallResult);
end;

function TEthereum.web3_sha3(Params: array of const;
  out CallResult: String): Boolean;
begin
  Result := RpcCallNew1<String>('web3_sha3', Params, CallResult);
end;

function TEthereum.RpcCallNew<T>(Method: String; Params: array of const;
  var CallResult: T): Boolean;
var
  JSV: TJSONValue;
begin
  Result := RpcCall(Method, Params);

  if Result then
    begin
      JSV := FMethodResult.GetValue('result');

      if JSV is TJSONObject then
        begin
          CallResult := Rest.Json.TJson.JsonToObject<T>(JSV as TJSONObject);
          Result := True;
        end else
      if JSV is TJSONArray then
        begin
          CallResult := Rest.Json.TJson.JsonToObject<T>(FMethodResult);
          Result := True;
        end;
    end;
end;

function TEthereum.admin_addPeer(const Url: String;
  out CallResult: Boolean): Boolean;
begin
  Result := RpcCallNew1<Boolean>('admin_addPeer', [eth_strToHex(Url)], CallResult);
end;

function TEthereum.admin_datadir(out CallResult: String): Boolean;
begin
  Result := RpcCallNew1<String>('admin_datadir', CallResult);
end;

function TEthereum.admin_nodeInfo(out CallResult: TAdmin_nodeInfoClass): Boolean;
begin
  Result := RpcCallNew<TAdmin_nodeInfoClass>('admin_nodeInfo', CallResult);
end;

function TEthereum.admin_peers(out CallResult: TAdmin_nodePeersClass): Boolean;
begin
  Result := RpcCallNew<TAdmin_nodePeersClass>('admin_peers', CallResult);
end;

function TEthereum.admin_setSolc(const Path: String;
  out CallResult: String): Boolean;
begin
  Result := RpcCallNew1<String>('admin_setSolc', CallResult);
end;

function TEthereum.admin_startRPC(const host: String; const port: Integer;
  const cors, apis: String; out CallResult: Boolean): Boolean;
begin
  Result := RpcCallNew1<Boolean>('admin_startRPC', [host, port, cors, apis], CallResult);
end;

function TEthereum.admin_startWS(const host: String; const port: Integer;
  const cors, apis: String; out CallResult: Boolean): Boolean;
begin
  Result := RpcCallNew1<Boolean>('admin_startWS', [host, port, cors, apis], CallResult);
end;

function TEthereum.admin_stopRPC(out CallResult: Boolean): Boolean;
begin
  Result := RpcCallNew1<Boolean>('admin_stopRPC', CallResult);
end;

function TEthereum.admin_stopWS(out CallResult: Boolean): Boolean;
begin
  Result := RpcCallNew1<Boolean>('admin_stopWS', CallResult);
end;

function TEthereum.net_listening(out CallResult: Boolean): Boolean;
begin
  Result := RpcCallNew1<Boolean>('net_listening', CallResult);
end;

function TEthereum.net_peerCount(out CallResult: Integer): Boolean;
var
  s: String;
begin
  Result := RpcCallNew1<String>('net_peerCount', s);
  if Result then
    CallResult := eth_hexToInt(s);
end;

function TEthereum.net_version(out CallResult: String): Boolean;
begin
  Result := RpcCallNew1<String>('net_version', CallResult);
end;

function TEthereum.personal_ecRecover(const message, signature: String;
  out CallResult: String): Boolean;
begin
  Result := RpcCallNew1<String>('personal_ecRecover', [eth_strToHex(message), eth_strToHex(signature)], CallResult);
end;

function TEthereum.personal_importRawKey(const keydata, passphrase: String;
  out CallResult: String): Boolean;
begin
  Result := RpcCallNew1<String>('personal_importRawKey', [eth_strToHex(keydata), eth_strToHex(passphrase)], CallResult);
end;

function TEthereum.personal_listAccounts(
  out CallResult: TArray<String>): Boolean;
var
  Accounts: TEth_StringArrayResultClass;
begin
  Result := RpcCallNew<TEth_StringArrayResultClass>('personal_listAccounts', Accounts);
  if Assigned(Accounts) then
    begin
      CallResult := Accounts.result;
      Accounts.Free;
    end;
end;

function TEthereum.personal_lockAccount(const address: String;
  out CallResult: Boolean): Boolean;
begin
  Result := RpcCallNew1<Boolean>('personal_lockAccount', [address], CallResult);
end;

function TEthereum.personal_newAccount(const password: String;
  out CallResult: String): Boolean;
begin
  Result := RpcCallNew1<String>('personal_newAccount', [eth_strToHex(password)], CallResult);
end;

function TEthereum.personal_sendTransaction(const from, &to, passphrase: String;
  const value: Int64; out CallResult: String): Boolean;
var
  ParamObject: TJSONObject;
begin
  ParamObject := TJSONObject.Create;
  try

    if from <> '' then ParamObject.AddPair(TJSONPair.Create('from', from));
    if &to <> '' then ParamObject.AddPair(TJSONPair.Create('to', &to));
    if value <> 0 then ParamObject.AddPair(TJSONPair.Create('value', eth_intToHex(value)));

    Result := RpcCallNew1<String>('personal_sendTransaction', [ParamObject, passphrase], CallResult);
  finally
    ParamObject.Free;
  end;
end;

function TEthereum.personal_sign(const message, account, password: String;
  out CallResult: String): Boolean;
begin
  Result := RpcCallNew1<String>('personal_sign', [eth_strToHex(message), account, password], CallResult);
end;

function TEthereum.personal_signAndSendTransaction(const from, &to: String;
  const gas, gasPrice, value: Int64; const data, password: String;
  out CallResult: String): Boolean;
var
  ParamObject: TJSONObject;
begin
  ParamObject := TJSONObject.Create;
  try

    if from <> '' then ParamObject.AddPair(TJSONPair.Create('from', from));
    if &to <> '' then ParamObject.AddPair(TJSONPair.Create('to', &to));
    if gas <> 0 then ParamObject.AddPair(TJSONPair.Create('gas', eth_intToHex(gas)));
    if gasPrice <> 0 then ParamObject.AddPair(TJSONPair.Create('gasPrice', eth_intToHex(gasPrice)));
    if value <> 0 then ParamObject.AddPair(TJSONPair.Create('value', eth_intToHex(value)));
    if data <> '' then ParamObject.AddPair(TJSONPair.Create('data', data));

    Result := RpcCallNew1<String>('personal_signAndSendTransaction', [ParamObject, password], CallResult);
  finally
    ParamObject.Free;
  end;
end;

function TEthereum.personal_unlockAccount(const address, passphrase: String;
  duration: Int64; out CallResult: Boolean): Boolean;
begin
    Result := RpcCallNew1<Boolean>('personal_unlockAccount', [address, eth_strToHex(passphrase), eth_intToHex(duration)], CallResult);
end;

{ TEthereumContract }

constructor TEthereumContract.Create;
begin
  inherited;
  FMethods := TObjectList<TEthereumContractMethod>.Create;
  FEvents := TObjectList<TEthereumContractEvent>.Create;
end;

class procedure TEthereumContract.DelphiForDynArrayConvertor(var EthName,
  EthType: String);
begin
  if pos('bytes', EthType) = 1 then
    begin
      DelphiSimpleConvertor(EthName, EthType);
      EthName := Format('eth_bytesToStr(%s)', [EthName]);
    end
  else
    DelphiSimpleConvertor(EthName, EthType);
end;

class procedure TEthereumContract.DelphiSimpleConvertor(var EthName,
  EthType: String);
//http://solidity.readthedocs.io/en/develop/types.html
begin
  if EthType = 'bool' then EthType := 'Boolean' else
  if EthType = 'string' then EthType := 'String' else

  if (pos('uint', EthType) = 1) or (pos('int', EthType) = 1) then
    case StrToIntDef(StringReplace(StringReplace(EthType, 'uint', '', []), 'int', '', []), 0) of
      8:      EthType := 'Byte';
      16:     EthType := 'Word';
      32:     EthType := 'Integer';
      33..64: EthType := 'Int64';
    end else

  if pos('bytes', EthType) = 1 then EthType := 'TByteDynArray' else

  EthType := 'TUnknown_' + EthType;
end;

destructor TEthereumContract.Destroy;
begin
  FMethods.Free;
  FEvents.Free;
  inherited;
end;

function TEthereumContract.ErrorLengthMismatch(Source: String;
  Param: TEthereumContractParameter): Boolean;
begin
  FreeAndNil(FMethodError);
  FMethodError := TEth_ErrorClass.Create;
  FMethodError.code := -1;
  FMethodError.message := Format('value length mismath on input parameter %s.%s:%s, value %s', [Source, Param.FName, Param.FType, Param.FValue]);
  Result := False;
  if not Result and Assigned(FOnMethodError) then FOnMethodError(Self);
end;

function TEthereumContract.GetEvent(EventName: String): TEthereumContractEvent;
var
  Event: TEthereumContractEvent;
begin
  Result := nil;
  for Event in FEvents do
    if Event.FEventName = EventName then
      begin
        Result := Event;
        Break;
      end;
end;

function TEthereumContract.GetEventHash(Event: TEthereumContractEvent): Boolean;
begin
  Result := Event.FEventHash <> '';

  if not Result then
    Result := web3_sha3([eth_strToHex(Event.EthereumEventSignature)], Event.FEventHash);
end;

function TEthereumContract.GetMethod(MethodName: String): TEthereumContractMethod;
var
  Method: TEthereumContractMethod;
begin
  Result := nil;
  for Method in FMethods do
    if Method.FMethodName = MethodName then
      begin
        Result := Method;
        Break;
      end;
end;

function TEthereumContract.ErrorBadCallCode(Source: String): Boolean;
begin
  FreeAndNil(FMethodError);
  FMethodError := TEth_ErrorClass.Create;
  FMethodError.code := -1;
  FMethodError.message := Format('Bad %s call code ', [Source.QuotedString]);
  Result := False;
  if not Result and Assigned(FOnMethodError) then FOnMethodError(Self);
end;

function TEthereumContract.BuildABI(out JSONString: String; const Pretty: Boolean = False;
  const IncludeValue: Boolean = False): Boolean;
var
  Method, Param: TJSONObject;
  Methods, Inputs, Outputs: TJSONArray;
  ContractEvent: TEthereumContractEvent;
  ContractMethod: TEthereumContractMethod;
  ContractParameter: TEthereumContractParameter;
begin
  Result := False;
  Methods := TJSONArray.Create;
  try
    for ContractMethod in FMethods do
      begin
        Method := TJSONObject.Create;

        Inputs := TJSONArray.Create;
        Outputs := TJSONArray.Create;

        Method.AddPair('inputs', Inputs);
        Method.AddPair('outputs', Outputs);
        Method.AddPair('type', TJSONString.Create(ContractMethod.FMethodType));
        Method.AddPair('name', TJSONString.Create(ContractMethod.FMethodName));
        Method.AddPair('constant', TJSONBool.Create(ContractMethod.FMethodConstant));
        Method.AddPair('payable', TJSONBool.Create(ContractMethod.FMethodPayable));

        for ContractParameter in ContractMethod.FInputs do
          begin
            Param := TJSONObject.Create;
            Param.AddPair('name', TJSONString.Create(ContractParameter.FName));
            Param.AddPair('type', TJSONString.Create(ContractParameter.FType));
            Inputs.AddElement(Param);
          end;

        for ContractParameter in ContractMethod.FOutputs do
          begin
            Param := TJSONObject.Create;
            Param.AddPair('name', TJSONString.Create(ContractParameter.FName));
            Param.AddPair('type', TJSONString.Create(ContractParameter.FType));
            Outputs.AddElement(Param);
          end;

        Methods.AddElement(Method);
      end;


    for ContractEvent in FEvents do
      begin
        Method := TJSONObject.Create;

        Inputs := TJSONArray.Create;

        Method.AddPair('inputs', Inputs);
        Method.AddPair('type', TJSONString.Create(ContractEvent.FEventType));
        Method.AddPair('name', TJSONString.Create(ContractEvent.FEventName));
        Method.AddPair('anonymous', TJSONBool.Create(ContractEvent.FEventAnonymous));

        for ContractParameter in ContractEvent.FParameters do
          begin
            Param := TJSONObject.Create;
            Param.AddPair('name', TJSONString.Create(ContractParameter.FName));
            Param.AddPair('type', TJSONString.Create(ContractParameter.FType));
            Param.AddPair('indexed', TJSONBool.Create(ContractParameter.FIndexed));
            Inputs.AddElement(Param);
          end;

        Methods.AddElement(Method);
      end;

      if Pretty
        then JSONString := TJson.Format(Methods)
        else JSONString := Methods.ToJSON;
    Result := True;
  finally
    Methods.Free;
  end;
end;

function TEthereumContract.BuildCallCode(Method: TEthereumContractMethod;
  Params: array of const; out Code: String): Boolean;
var
  s: String;
  i: Integer;
  Param: TEthereumContractParameter;
begin
  FreeAndNil(FMethodError);
  Result := False;

  if Length(Params) <> Method.FInputs.Count then
    begin
      ErrorNotEnoughParameters(Method.FMethodName);
      Exit;
    end;

  for Param in Method.FInputs do
    begin
      i := Method.FInputs.IndexOf(Param);
      case Params[i].VType of
        vtAnsiString:
          if Param.FType <> 'string' then
            begin
              ErrorTypeCast(Method.FMethodName, Param, 'vtAnsiString');
              Exit;
            end else
          Param.SetAsString(String(AnsiString(TVarRec(Params[i]).VAnsiString)));

        vtUnicodeString:
          if Param.FType <> 'string' then
            begin
              ErrorTypeCast(Method.FMethodName, Param, 'vtUnicodeString');
              Exit;
            end else
          Param.SetAsString(String(UnicodeString(TVarRec(Params[i]).VUnicodeString)));

        vtInteger:
          if (Param.FType <> 'int8')
          or (Param.FType <> 'int32')
          or (Param.FType <> 'uint32') then
            begin
              ErrorTypeCast(Method.FMethodName, Param, 'vtInteger');
              Exit;
            end else
          Param.SetAsInteger(TVarRec(Params[i]).VInteger);

        vtInt64:
          if (Param.FType <> 'int64')
          or (Param.FType <> 'uint64') then
            begin
              ErrorTypeCast(Method.FMethodName, Param, 'vtInt64');
              Exit;
            end else
          Param.SetAsInt64(TVarRec(Params[i]).VInt64^);

        vtBoolean:
          if Param.FType <> 'boolean' then
            begin
              ErrorTypeCast(Method.FMethodName, Param, 'vtBoolean');
              Exit;
            end else
          Param.SetAsBoolean(TVarRec(Params[i]).VBoolean);

        vtObject: //BigInt
          begin
            ErrorTypeCast(Method.FMethodName, Param, 'vtObject');
            Exit;
          end;
      end;
    end;
  Result := BuildCallCode(Method, Code);
end;

function TEthereumContract.BuildCallCode(Method: TEthereumContractMethod;
  out Code: String): Boolean;
var
  s, Hash: String;
  i, ParamCount: Integer;
  Param: TEthereumContractParameter;
  StaticPart, DynamicPart: TList<String>;
begin
  FreeAndNil(FMethodError);
  Result := GetMethodHash(Method);

  if Result then
    try
      StaticPart := TList<String>.Create;
      DynamicPart := TList<String>.Create;

      for Param in Method.FInputs do
        if Param.FValue.Length <> eth_len * 2 then
          begin
            Result := ErrorLengthMismatch(Method.FMethodName, Param);
            Exit;
          end else
        begin
          i := Method.FInputs.IndexOf(Param);
          if (Param.FType = 'int8')
          or (Param.FType = 'int32')
          or (Param.FType = 'uint32')
          or (Param.FType = 'int64')
          or (Param.FType = 'uint64')
          or (Param.FType = 'boolean') then
            begin
              StaticPart.Add(Param.FValue);
            end else
          if Param.FType = 'string' then
            begin
              StaticPart.Add(eth_intToHex(Method.FInputs.Count * eth_len, False, -1));
              DynamicPart.Add(Param.FValue);
            end;
        end;

      Code := eth_hex + Method.FMethodHash;
      for s in StaticPart do Code := Code + s;
      for s in DynamicPart do Code := Code + s;

      Result := True;
  finally
    StaticPart.Free;
    DynamicPart.Free;
  end;
end;

function TEthereumContract.BuildDelphiSource(ContractName: String;
  Output: TStrings): Boolean;
var
  i, j: Integer;
  IntPart, ImpPart: TStringList;
  ParName, ParType: String;
  Methods, Inputs, Outputs: TJSONArray;
  Method: TJSONObject;
  Initial: TJSONValue;
  int, imp: TStringList;
  ContractEvent: TEthereumContractEvent;
  ContractMethod: TEthereumContractMethod;
  Parameter: TEthereumContractParameter;
begin
  int := TStringList.Create;
  imp := TStringList.Create;
  IntPart := TStringList.Create;
  ImpPart := TStringList.Create;

  try
    IntPart.Add('unit ' + ContractName + ';');
    IntPart.Add('');
    IntPart.Add(Format('// auto generated source at %s', [DateTimeToStr(Now)]));
    IntPart.Add('');
    IntPart.Add('interface');
    IntPart.Add('');
    IntPart.Add('uses System.Types, System.SysUtils, Generics.Collections, Rest.Json,');
    IntPart.Add('  DEthereum, DEthereum.Types;');
    IntPart.Add('');
    IntPart.Add('type');

    ContractName := 'TEth_Contract' + ContractName;

    int.Add(Format('  %s = class(TEthereumContract)', [ContractName]));
    int.Add('  private');
    int.Add('    procedure InitContract;');
    int.Add('  public');
    int.Add('    constructor Create;');

    imp.Clear;
    imp.add(Format('constructor %s.Create;', [ContractName]));
    imp.add('begin');
    imp.add('  inherited;');
    imp.add('  InitContract;');
    if ContractAddress = '' then
      ContractAddress := 'UNKOWN_ADDRESS';
    imp.add(Format('  ContractAddress := %s;', [ContractAddress.QuotedString]));
    imp.add('end;');
    imp.add('');
    ImpPart.AddStrings(imp);

    imp.Clear;
    imp.Add(Format('procedure %s.InitContract;', [ContractName]));
    imp.Add('var');
    imp.Add('  Event: TEthereumContractEvent;');
    imp.Add('  Method: TEthereumContractMethod;');
    imp.Add('  Parameter: TEthereumContractParameter;');
    imp.Add('begin');

    for ContractMethod in FMethods do
      begin
        if ContractMethod.FMethodType = 'function' then
          begin
            int.Add(ContractMethod.DelphiMethodSignature(''));

            imp.Add('');
            imp.Add('  Method := TEthereumContractMethod.Create;');
            imp.Add(Format('  Method.MethodName := %s;', [ContractMethod.MethodName.QuotedString]));
            imp.Add(Format('  Method.MethodType := %s;', [ContractMethod.MethodType.QuotedString]));
            imp.Add(Format('  Method.MethodConstant := %s;', [BoolToStr(ContractMethod.MethodConstant, True)]));//boolean helper ContractMethod.MethodConstant.ToString(True) have bug: always return numeric value!!!
            imp.Add(Format('  Method.MethodHash := %s;', [ContractMethod.MethodHash.QuotedString]));

            for Parameter in ContractMethod.FInputs do
              begin
                imp.Add('');
                imp.Add('  Parameter := TEthereumContractParameter.Create;');
                imp.Add(Format('  Parameter.Name := %s;', [Parameter.FName.QuotedString]));
                imp.Add(Format('  Parameter.&Type := %s;', [Parameter.FType.QuotedString]));
                imp.Add('  Method.Inputs.Add(Parameter);');
              end;

            for Parameter in ContractMethod.FOutputs do
              begin
                imp.Add('');
                imp.Add('  Parameter := TEthereumContractParameter.Create;');
                imp.Add(Format('  Parameter.Name := %s;', [Parameter.FName.QuotedString]));
                imp.Add(Format('  Parameter.&Type := %s;', [Parameter.FType.QuotedString]));
                imp.Add('  Method.Outputs.Add(Parameter);');
              end;

            imp.Add('  Methods.Add(Method);');
          end;
      end;

    for ContractEvent in FEvents do
      begin
        int.Add(ContractEvent.DelphiEventSignature(''));

        imp.Add('');
        imp.Add('  Event := TEthereumContractEvent.Create;');
        imp.Add(Format('  Event.EventName := %s;', [ContractEvent.FEventName.QuotedString]));
        imp.Add(Format('  Event.EventType := %s;', [ContractEvent.FEventType.QuotedString]));
        imp.Add(Format('  Event.EventHash := %s;', [ContractEvent.FEventHash.QuotedString]));
        imp.Add(Format('  Event.EventAnonymous := %s;', [BoolToStr(ContractEvent.FEventAnonymous, True)]));

        for Parameter in ContractEvent.FParameters do
          begin
            imp.Add('');
            imp.Add('  Parameter := TEthereumContractParameter.Create;');
            imp.Add(Format('  Parameter.Name := %s;', [Parameter.FName.QuotedString]));
            imp.Add(Format('  Parameter.&Type := %s;', [Parameter.FType.QuotedString]));
            imp.Add(Format('  Parameter.Indexed := %s;', [BoolToStr(Parameter.FIndexed, True)]));
            imp.Add('  Event.Parameters.Add(Parameter);');
          end;

        imp.Add('  Events.Add(Event);');
      end;

    imp.Add('end;');
    ImpPart.AddStrings(imp);

    imp.Clear;
    for ContractMethod in FMethods do
      begin
        imp.Add('');
        imp.Add(ContractMethod.DelphiMethodSignature(ContractName));
      end;
    for ContractEvent in FEvents do
      begin
        imp.Add('');
        imp.Add(ContractEvent.DelphiEventSignature(ContractName));
      end;
    ImpPart.AddStrings(imp);

    int.Add('  end;');
    int.Add('');

    IntPart.AddStrings(int);
    IntPart.Add('implementation');
    IntPart.AddStrings(ImpPart);
    IntPart.Add('');
    IntPart.Add('end.');

    if Assigned(Output) then
      begin
        Output.Clear;
        Output.AddStrings(IntPart);
      end;
  finally
    Initial.Free;
    IntPart.Free;
    ImpPart.Free;
    int.Free;
  end;
end;

function TEthereumContract.GetMethodHash(Method: TEthereumContractMethod): Boolean;
begin
  Result := Method.FMethodHash <> '';

  if not Result then
    if web3_sha3([eth_strToHex(Method.EthereumMethodSignature)], Method.FMethodHash) then
      begin
        Result := True;
        Method.FMethodHash := Copy(Method.FMethodHash, 3, 8);
      end;
end;

class function TEthereumContract.NamesTypes(
  Parameters: TObjectList<TEthereumContractParameter>; Prefix,
  ParameterDelimiter, TypeSeparator: String; IncludeName, IncludeType: Boolean;
  Convertor: TNameTypeConvertor): String;
var
  n, t: String;
  p: TEthereumContractParameter;
begin
  Result := '';
  if IncludeName or IncludeType then
    for p in Parameters do
      begin
        if Result <> '' then
          Result := Result + ParameterDelimiter;

        if Prefix <> '' then
          Result := Result + Prefix;

        n := p.FName;
        t := p.FType;
        if Assigned(Convertor) then
          Convertor(n, t);

        if (TypeSeparator <> '') and IncludeName and IncludeType then
          Result := Result + n + TypeSeparator + t else

        if IncludeName then
          Result := Result + n else

        if IncludeType then
          Result := Result + t;
      end;
end;

function TEthereumContract.ErrorNotFound(Source: String): Boolean;
begin
  FreeAndNil(FMethodError);
  FMethodError := TEth_ErrorClass.Create;
  FMethodError.code := -1;
  FMethodError.message := Format('Method %s not found', [Source.QuotedString]);
  Result := False;
  if not Result and Assigned(FOnMethodError) then FOnMethodError(Self);
end;

function TEthereumContract.ErrorNotEnoughParameters(
  Source: String): Boolean;
begin
  FreeAndNil(FMethodError);
  FMethodError := TEth_ErrorClass.Create;
  FMethodError.code := -1;
  FMethodError.message := Format('Is not enough parameters for %s', [Source.QuotedString]);
  Result := False;
  if not Result and Assigned(FOnMethodError) then FOnMethodError(Self);
end;

function TEthereumContract.ErrorParamConvert(Source: String; E: Exception): Boolean;
begin
  FreeAndNil(FMethodError);
  FMethodError := TEth_ErrorClass.Create;
  FMethodError.code := -1;
  FMethodError.message := Format('Parameter convert error %s on %s', [E.Message, Source.QuotedString]);
  Result := False;
  if not Result and Assigned(FOnMethodError) then FOnMethodError(Self);
end;

class function TEthereumContract.DelphiParametersAssigner(
  Parameters: TObjectList<TEthereumContractParameter>;
  ParamNamePrefix: String): String;
var
  i: Integer;
  n, t: String;
  p: TEthereumContractParameter;
begin
  Result := '';
  for i := 0 to Parameters.Count - 1 do
    begin
      p := Parameters[i];
      n := p.FName;
      t := p.FType;

      DelphiSimpleConvertor(n, t);

      if t = 'Boolean' then t := 'AsBoolean' else
      if t = 'String' then t := 'AsString' else
      if t = 'Byte' then t := 'AsByte' else
      if t = 'Integer' then t := 'AsInteger' else
      if t = 'UInt32' then t := 'AsUInt32' else
      if t = 'Int64' then t := 'AsInt64' else
      if t = 'UInt64' then t := 'AsUInt64' else
      if t = 'TByteDynArray' then t := 'AsBytes' else
      t := 'AsUNKNOWN';

      Result := Result + Format('          %s := %s[%d].%s;', [n, ParamNamePrefix, i, t]) + sLineBreak;
    end;
end;

function TEthereumContract.ParseABI(const JSONString: String): Boolean;

    procedure ParametersFromArray(&Array: TJSONArray; Parameters: TObjectList<TEthereumContractParameter>; PrefixName, EmptyName: String);
    var
      i: Integer;
      jsv: TJSONValue;
      cmp: TEthereumContractParameter;
    begin
      for i := 0 to &Array.Count - 1 do
        begin
          jsv := &Array.Items[i];
          cmp := TEthereumContractParameter.Create;
          cmp.FName := jsv.GetValue<String>('name', '');
          cmp.FType := jsv.GetValue<String>('type', '');
          cmp.FIndexed := jsv.GetValue<Boolean>('indexed', False);

          if cmp.FName = '' then
            if &Array.Count = 1
              then cmp.FName := Format('%s%s', [PrefixName, EmptyName])
              else cmp.FName := Format('%s%d', [PrefixName, i]);

          Parameters.Add(cmp);
        end;
    end;

var
  i: Integer;
  MethodType, MethodName, ParName: String;
  Methods, Inputs, Outputs: TJSONArray;
  Metadata, Hashes, Method: TJSONObject;
  Initial: TJSONValue;
  ContractEvent: TEthereumContractEvent;
  ContractMethod: TEthereumContractMethod;
  MethodParameter: TEthereumContractParameter;
begin
  Result := False;
  try
    Hashes := nil;
    Initial := nil;
    Methods := nil;
    try
      Initial := TJSONObject.ParseJSONValue(JSONString);

      if Initial is TJSONArray then //pure ABI array
        begin
          Methods := Initial as TJSONArray;
         end else

      if Initial is TJSONObject then
        begin
          Metadata := Initial as TJSONObject;

          if Metadata.TryGetValue<String>('abi', ParName) then
            begin
              Methods := TJSONObject.ParseJSONValue(ParName) as TJSONArray;
              Metadata.AddPair('methods', Methods);

              Metadata.TryGetValue<TJSONObject>('functionHashes', Hashes);
              Metadata.TryGetValue<String>('contractAddress', FContractAddress);
            end else

          if (Metadata.Count = 1) and (Metadata.Pairs[0].JsonValue is TJSONObject) //"<stdin>:Photo":{
          and (Metadata.Pairs[0].JsonValue as TJSONObject).TryGetValue<TJSONObject>('info', Method)
          and Method.TryGetValue<TJSONArray>('abiDefinition', Methods) then
            begin
              Method.TryGetValue<TJSONArray>('abiDefinition', Methods);
            end else;
        end;

      if Assigned(Methods) then
      for i := 0 to Methods.Count - 1 do
        if Methods.Items[i] is TJSONObject then
          begin
            Method := Methods.Items[i] as TJSONObject;

            Inputs := Method.Values['inputs'] as TJSONArray;
            Outputs := Method.Values['outputs'] as TJSONArray;

            MethodType := (Method.Values['type'] as TJSONString).Value;
            MethodName := (Method.Values['name'] as TJSONString).Value;

            if MethodType = 'constructor' then
              begin
                // not supported contract constructor
              end else

            if MethodType = 'function' then
              begin
                ContractMethod := TEthereumContractMethod.Create;
                ContractMethod.FMethodType := MethodType;
                ContractMethod.FMethodName := MethodName;
                ContractMethod.FMethodConstant := (Method.Values['constant'] as TJSONBool).AsBoolean;
                ContractMethod.FMethodPayable := (Method.Values['payable'] as TJSONBool).AsBoolean;

                if Assigned(Inputs) then
                  ParametersFromArray(Inputs, ContractMethod.Inputs, '_', ContractMethod.FMethodName);
                if Assigned(Outputs) then
                  ParametersFromArray(Outputs, ContractMethod.Outputs, '__', ContractMethod.FMethodName);
                if Assigned(Hashes) then
                  Hashes.TryGetValue<string>(ContractMethod.EthereumMethodSignature, ContractMethod.FMethodHash);

                FMethods.Add(ContractMethod);
              end else

            if MethodType = 'event' then
              begin
                ContractEvent := TEthereumContractEvent.Create;
                ContractEvent.FEventType := MethodType;
                ContractEvent.FEventName := MethodName;
                ContractEvent.FEventAnonymous := (Method.Values['anonymous'] as TJSONBool).AsBoolean;

                if Assigned(Inputs) then
                  ParametersFromArray(Inputs, ContractEvent.Parameters, '_', ContractEvent.FEventName);

                //disabled hash load, because jsons hash has truncated length
                if False and Assigned(Hashes) then
                  Hashes.TryGetValue<string>(ContractEvent.EthereumEventSignature, ContractEvent.FEventHash);

                FEvents.Add(ContractEvent);
              end;
          end;
        Result := True;
    except
      on E: Exception do
        begin
          raise E;
          //Result := MethodErr(E);
          Exit;
        end;
    end;
  finally
    FreeAndNil(Initial);
  end;
end;

function TEthereumContract.ParseCallCode(Source, Code: String;
  Destination: TObjectList<TEthereumContractParameter>): Boolean;
var
  i: Integer;
  Parts: TList<String>;
  p: TEthereumContractParameter;
begin
  Code := ReplaceStr(Code, eth_hex, '');

  if (Code.Length = 0) or (Code.Length mod eth_len <> 0) then
    begin
      Result := ErrorBadCallCode(Source);
      Exit;
    end;

  try
    Parts := TList<String>.Create;
    while Code.Length > 0 do
      begin
        Parts.Add(Copy(Code, 1, eth_len));
        Delete(Code, 1, eth_len);
      end;

    for p in Destination do
      if (p.FType = 'bool')
      or (p.FType = 'int32')
      or (p.FType = 'int64')
      or (p.FType = 'uint64') then
        begin
          p.FValue := Parts[Destination.IndexOf(p)];
        end else
      if p.FType = 'string' then
        begin
          i := eth_hexToInt(Parts[Destination.IndexOf(p)]) div eth_len;
          if i > Destination.Count then  //if offset more than parts.count
            begin
              Result := ErrorBadCallCode(Source);
              Exit;
            end;
          p.FValue := Parts[i];
        end else
        begin
          Result := ErrorTypeCast(Source, p, p.FType);
          Exit;
        end;

      Result := True;
  finally
    Parts.Free;
  end;
end;

function TEthereumContract.ErrorTypeCast(Source: String;
  Param: TEthereumContractParameter; PassedType: String): Boolean;
begin
  FreeAndNil(FMethodError);
  FMethodError := TEth_ErrorClass.Create;
  FMethodError.code := -1;
  FMethodError.message := Format('type cast error on input parameter %s(%s): %s with %s', [Source, Param.FName, Param.FType, PassedType]);
  Result := False;
  if not Result and Assigned(FOnMethodError) then FOnMethodError(Self);
end;

{ TEthereumContractMethod }

constructor TEthereumContractMethod.Create;
begin
  inherited;
  FInputs := TObjectList<TEthereumContractParameter>.Create;
  FOutputs := TObjectList<TEthereumContractParameter>.Create;
end;

function TEthereumContractMethod.DelphiMethodSignature(ContractClassName: String): String;
var
  i: Integer;
  Inputs, Outputs, n, t: String;
  Param: TEthereumContractParameter;
begin
  Inputs := TEthereumContract.NamesTypes(FInputs, 'const ', '; ', ': ', True, True, TEthereumContract.DelphiSimpleConvertor);
  Outputs := TEthereumContract.NamesTypes(FOutputs, 'out ', '; ', ': ', True, True, TEthereumContract.DelphiSimpleConvertor);

  Result := 'const gas, gasPrice: Int64';

  if Inputs <> '' then
    Result := Result + '; ';
  Result := Result + Inputs;

  if Outputs <> '' then
    Result := Result + '; ';
  Result := Result + Outputs;

  if ContractClassName <> '' then
    begin
      Inputs := '';
      Outputs := '';

      Result := Format(
        'function %s.%s(%s): Boolean;' + sLineBreak +
        'var' + sLineBreak +
        '  Code, CallResult: String;' + sLineBreak +
        '  Method: TEthereumContractMethod;' + sLineBreak +
        'begin' + sLineBreak +
        '  Method := GetMethod(%s);' + sLineBreak +
        '  if Assigned(Method) then' + sLineBreak +
        '    if Method.MethodConstant' + sLineBreak +
        '      then Result := BuildCallCode(Method, [%s], Code) and eth_call(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, ethbnLatest, 0, CallResult)' + sLineBreak +
        '      else Result := BuildCallCode(Method, [%s], Code) and personal_signAndSendTransaction(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, CoinPassword, CallResult)' + sLineBreak +
        '  else' + sLineBreak +
        '  begin' + sLineBreak +
        '    Result := ErrorNotFound(%s);' + sLineBreak +
        '    Exit;' + sLineBreak +
        '  end;' + sLineBreak +
        '  if Result and (Method.Outputs.Count > 0) then' + sLineBreak +
        '    begin' + sLineBreak +
        '      Result := ParseCallCode(Method.MethodName, CallResult, Method.Outputs);' + sLineBreak +
        '      if Result then' + sLineBreak +
        '        try' + sLineBreak +
        '%s' +
        '        except' + sLineBreak +
        '        on E: Exception do' + sLineBreak +
        '          Result := ErrorParamConvert(Method.MethodName, E);' + sLineBreak +
        '        end' + sLineBreak +
        '    end;' + sLineBreak +
        'end;',
        [
          ContractClassName, FMethodName, Result,
          FMethodName.QuotedString,
          TEthereumContract.NamesTypes(FInputs, '', ', ', '', True, False, TEthereumContract.DelphiForDynArrayConvertor),
          TEthereumContract.NamesTypes(FInputs, '', ', ', '', True, False, TEthereumContract.DelphiForDynArrayConvertor),
          FMethodName.QuotedString,
          TEthereumContract.DelphiParametersAssigner(FOutputs, 'Method.Outputs')//Outputs
        ]);
    end else
  Result := Format('    function %s(%s): Boolean;', [FMethodName, Result]);
end;

destructor TEthereumContractMethod.Destroy;
begin
  FInputs.Free;
  FOutputs.Free;
  inherited;
end;

function TEthereumContractMethod.EthereumMethodSignature: String;
begin
  Result := Format('%s(%s)', [FMethodName, TEthereumContract.NamesTypes(FInputs, '', ',', '', False, True, nil)]);
end;

{ TEthereumContractParameter }

function TEthereumContractParameter.GetAsBoolean: Boolean;
begin
  Result := StrToBool(FValue);
end;

function TEthereumContractParameter.GetAsByte: Byte;
begin
  Result := StrToInt('$' + FValue);
end;

function TEthereumContractParameter.GetAsBytes: TByteDynArray;
begin
  Result := eth_hexToBytes(FValue);
end;

function TEthereumContractParameter.GetAsInt64: Int64;
begin
  Result := StrToInt64('$' + FValue);
end;

function TEthereumContractParameter.GetAsInteger: Integer;
begin
  Result := StrToInt('$' + FValue);
end;

function TEthereumContractParameter.GetAsString: String;
begin
  Result := eth_hexToStr(FValue);
end;

function TEthereumContractParameter.GetAsUInt32: UInt32;
begin
  Result := StrToUInt('$' + FValue);
end;

function TEthereumContractParameter.GetAsUInt64: UInt64;
begin
  Result := StrToUInt64('$' + FValue);
end;

procedure TEthereumContractParameter.SetAsBoolean(const Value: Boolean);
begin
  FValue := eth_booleanToHex(Value, False, -eth_len);
end;

procedure TEthereumContractParameter.SetAsByte(const Value: Byte);
begin
  FValue := eth_intToHex(Value, False, -eth_len);
end;

procedure TEthereumContractParameter.SetAsBytes(const Value: TByteDynArray);
begin
  FValue := eth_bytesToHex(Value, False, -eth_len);
end;

procedure TEthereumContractParameter.SetAsInt64(const Value: Int64);
begin
  FValue := eth_intToHex(Value, False, -eth_len);
end;

procedure TEthereumContractParameter.SetAsInteger(const Value: Integer);
begin
  FValue := eth_intToHex(Value, False, -eth_len);
end;

procedure TEthereumContractParameter.SetAsString(const Value: String);
begin
  FValue := eth_strToHex(Value, False, eth_len);
end;

procedure TEthereumContractParameter.SetAsUInt32(const Value: UInt32);
begin
  FValue := eth_uintToHex(Value, False, -eth_len);
end;

procedure TEthereumContractParameter.SetAsUInt64(const Value: UInt64);
begin
  FValue := eth_uintToHex(Value, False, -eth_len);
end;

{ TEthereumContractEvent }

constructor TEthereumContractEvent.Create;
begin
  inherited;
  FEvents := TObjectList<TEth_FilterChangeClass>.Create;  
  FParameters := TObjectList<TEthereumContractParameter>.Create;
end;

function TEthereumContractEvent.DelphiEventSignature(
  ContractClassName: String): String;
var
  Filter, Get: String;
begin
  Filter := 'const FromBlockNumber: TEth_BlockNumber; const FromBlockNumberCustom: Int64; const ToBlockNumber: TEth_BlockNumber; const ToBlockNumberCustom: Int64';
  Get := 'const Index: Int64';
  if FParameters.Count > 0 then
    Get := Get + '; ' + TEthereumContract.NamesTypes(FParameters, 'out ', '; ', ': ', True, True, TEthereumContract.DelphiSimpleConvertor);

  if ContractClassName <> '' then
    Result := Format(
      'function %s.FilterEvent_%s(%s): Boolean;' + sLineBreak +
      'var' + sLineBreak +
      '  Topics: TArray<String>;' + sLineBreak +
      '  Event: TEthereumContractEvent;' + sLineBreak +
      'begin' + sLineBreak +
      '  Event := GetEvent(%s);' + sLineBreak +
      '  if Assigned(Event) then' + sLineBreak +
      '    begin' + sLineBreak +
      '      Result := GetEventHash(Event);' + sLineBreak +
      '      if Result then' + sLineBreak +
      '        begin' + sLineBreak +
      '          SetLength(Topics, 1);' + sLineBreak +
      '          Topics[0] := Event.EventHash;' + sLineBreak +
      '          Event.Events.Clear;' + sLineBreak +
      '          Result := eth_getLogs(FromBlockNumber, FromBlockNumberCustom, ToBlockNumber, ToBlockNumberCustom, ContractAddress, Topics, Event.Events);' + sLineBreak +
      '        end;' + sLineBreak +
      '    end else' + sLineBreak +
      '    Result := ErrorNotFound(%s);' + sLineBreak +
      'end;'  + sLineBreak +

      sLineBreak +

      'function %s.GetEvent_%s(%s): Boolean;' + sLineBreak +
      'var' + sLineBreak +
      '  Event: TEthereumContractEvent;' + sLineBreak +
      'begin' + sLineBreak +
      '  Event := GetEvent(%s);' + sLineBreak +
      '  if Assigned(Event) then' + sLineBreak +
      '    try' + sLineBreak +
      '      Result := ParseCallCode(Event.EventName, Event.Events[Index].Data, Event.Parameters);' + sLineBreak +
      '      if Result then' + sLineBreak +
      '        begin' + sLineBreak +
      '%s' +
      '        end;' + sLineBreak +
      '    except' + sLineBreak +
      '      on E: Exception do' + sLineBreak +
      '        Result := ErrorParamConvert(Event.EventName, E);' + sLineBreak +
      '    end else' + sLineBreak +
      '    Result := ErrorNotFound(%s);' + sLineBreak +
      'end;'  + sLineBreak,
      [
        ContractClassName, FEventName, Filter,
        FEventName.QuotedString,
        FEventName.QuotedString,

        ContractClassName, FEventName, Get,
        FEventName.QuotedString,
        TEthereumContract.DelphiParametersAssigner(FParameters, 'Event.Parameters'),
        FEventName.QuotedString
      ]) else

  Result := Format(
    '    function FilterEvent_%s(%s): Boolean;' + sLineBreak +
    '    function GetEvent_%s(%s): Boolean;',
    [
      FEventName, Filter,
      FEventName, Get
    ]);
end;

destructor TEthereumContractEvent.Destroy;
begin  
  FEvents.Free;
  FParameters.Free;
  inherited;
end;

function TEthereumContractEvent.EthereumEventSignature: String;
begin
  Result := Format('%s(%s)', [FEventName, TEthereumContract.NamesTypes(FParameters, '', ',', '', False, True, nil)]);
end;

end.
