unit frmMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, Generics.Collections,
  REST.Json,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.StdCtrls, FMX.TabControl, FMX.EditBox, FMX.NumberBox, FMX.Edit, FMX.Ani,
  FMX.ListBox, System.Rtti, FMX.Grid.Style, FMX.Grid, Data.Bind.Controls,
  FMX.Layouts, Fmx.Bind.Navigator, Data.Bind.Components, Data.Bind.DBScope,
  Data.DB, Data.Bind.EngExt, Fmx.Bind.DBEngExt, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.StorageBin,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  DEthereum, DEthereum.Types, DemoContract, System.Math.Vectors, FMX.Controls3D,
  FMX.Objects3D, FMX.Menus;

type
  TformMain = class(TForm)
    TimerHashRate: TTimer;
    TabControl1: TTabControl;
    TabItemLog: TTabItem;
    PanelSetup: TPanel;
    EditServer: TEdit;
    NumberBoxPort: TNumberBox;
    FloatAnimation1: TFloatAnimation;
    ComboBoxFromAddress: TComboBox;
    EditFromPassword: TEdit;
    PasswordEditButton1: TPasswordEditButton;
    ButtonStartMiner: TButton;
    ButtonStopMiner: TButton;
    CheckBoxHashRate: TCheckBox;
    StringGridLog: TStringGrid;
    TimeColumn1: TTimeColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    TabItemTextLog: TTabItem;
    MemoTextLog: TMemo;
    TabItemABI: TTabItem;
    TabControlABI: TTabControl;
    TabItemABIFull: TTabItem;
    MemoABIFull: TMemo;
    TabItemABIShort: TTabItem;
    MemoABIShort: TMemo;
    TabItemABICustom: TTabItem;
    MemoABICustom: TMemo;
    TabItemABIResult: TTabItem;
    TabControl2: TTabControl;
    TabItemDelphiSource: TTabItem;
    MemoABIDelphiSource: TMemo;
    ButtonSaveDelphi: TButton;
    TabItemABIBuildResult: TTabItem;
    MemoContractABI: TMemo;
    EditContractAddress: TEdit;
    ButtonProcessABI: TButton;
    TabItemSolidity: TTabItem;
    ButtonCompileSolidity: TButton;
    TabControlSolidity: TTabControl;
    TabItemSoliditySource: TTabItem;
    MemoSoliditySource: TMemo;
    TabItemSolidityResult: TTabItem;
    MemoSolidityResult: TMemo;
    TabTransaction: TTabItem;
    PanelTransaction: TPanel;
    Block: TTabItem;
    Panel1: TPanel;
    EditBlock: TEdit;
    EditButtonBlock: TEditButton;
    TabWatch: TTabItem;
    TabControl3: TTabControl;
    TabEvents: TTabItem;
    StringGridEvents: TStringGrid;
    EventName: TStringColumn;
    EventValues: TStringColumn;
    TabMethods: TTabItem;
    StringGridMethods: TStringGrid;
    StringColumn7: TStringColumn;
    StringColumn8: TStringColumn;
    StringColumn9: TStringColumn;
    StringColumn10: TStringColumn;
    StringColumn11: TStringColumn;
    TabState: TTabItem;
    StringGridState: TStringGrid;
    StringColumn12: TStringColumn;
    StringColumn13: TStringColumn;
    StringColumn16: TStringColumn;
    TabIABI: TTabItem;
    PathLabel1: TPathLabel;
    FloatAnimation2: TFloatAnimation;
    MemoWatchABI: TMemo;
    StatusBar: TStatusBar;
    LabelError: TLabel;
    EditWatchAddress: TEdit;
    StringColumn14: TStringColumn;
    StringColumn15: TStringColumn;
    SearchEditButton1: TSearchEditButton;
    TabItem1: TTabItem;
    EditAddress: TEdit;
    StringGridTransactions: TStringGrid;
    StringColumn17: TStringColumn;
    StringColumn18: TStringColumn;
    SearchTransactions: TSearchEditButton;
    PopupMenuNew: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure ButtonProcessABIClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EditServerChange(Sender: TObject);
    procedure ButtonCompileSolidityClick(Sender: TObject);
    procedure ComboBoxFromAddressClick(Sender: TObject);
    procedure ButtonStartMinerClick(Sender: TObject);
    procedure ButtonStopMinerClick(Sender: TObject);
    procedure TimerHashRateTimer(Sender: TObject);
    procedure CallCodeButtonClick(Sender: TObject);
    procedure ButtonSaveDelphiClick(Sender: TObject);
    procedure TabEventsClick(Sender: TObject);
    procedure StringGridEventsCellClick(const Column: TColumn;
      const Row: Integer);
    procedure TabMethodsClick(Sender: TObject);
    procedure TabStateClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MemoWatchABIChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
    FEth: TEthereum;
    demo: TEth_ContractDemoContract;
    FWatch: TEth_ContractDemoContract;
    procedure OnMethodError(Sender: TObject);
    procedure OnMethodCall(Sender: TObject);
    procedure OnMethodResult(Sender: TObject);
    procedure AddLog(&Type, Text: String);
    procedure SetupEthereum(Eth: TEthereum);
  public
    { Public declarations }
  end;

var
  Eth: TEthereum;
  formMain: TformMain;

procedure InfoToStringGrid(Info: TArray<String>; StringGrid: TStringGrid; ClearBefore: Boolean);
procedure RecordToStringGrid(Info: TArray<String>; StringGrid: TStringGrid; ClearBefore: Boolean);

implementation

uses
  frmTransaction,
  frmBlock;

procedure InfoToStringGrid(Info: TArray<String>; StringGrid: TStringGrid;
  ClearBefore: Boolean);
var
  i: Integer;
begin
  if ClearBefore then
    StringGrid.RowCount := 0;

  for i := 0 to (Length(Info) - 1) div 2 do
    begin
      StringGrid.RowCount := StringGrid.RowCount + 1;
      StringGrid.Cells[0, StringGrid.RowCount - 1] := Info[i * 2];
      StringGrid.Cells[1, StringGrid.RowCount - 1] := Info[i * 2 + 1];
    end;
end;

procedure RecordToStringGrid(Info: TArray<String>; StringGrid: TStringGrid;
  ClearBefore: Boolean);
var
  i: Integer;
begin
  if ClearBefore then
    StringGrid.RowCount := 0;

  StringGrid.RowCount := StringGrid.RowCount + 1;
  for i := 0 to (Length(Info) - 1) do
    StringGrid.Cells[i, StringGrid.RowCount - 1] := Info[i];
end;

{$R *.fmx}

procedure TformMain.AddLog(&Type, Text: String);
begin
  StringGridLog.RowCount := StringGridLog.RowCount + 1;
  StringGridLog.Cells[0, StringGridLog.RowCount - 1] := DateTimeToStr(Now);
  StringGridLog.Cells[1, StringGridLog.RowCount - 1] := &Type;
  StringGridLog.Cells[2, StringGridLog.RowCount - 1] := Text;

  MemoTextLog.Lines.Add(DateTimeToStr(Now));
  MemoTextLog.Lines.Add(&Type);
  MemoTextLog.Lines.Add(Text);

  MemoTextLog.Lines.Add('');
  MemoTextLog.Lines.Add('');
  MemoTextLog.Lines.Add('');
end;

procedure TformMain.ButtonCompileSolidityClick(Sender: TObject);
var
  s: String;
begin
  MemoSolidityResult.Lines.Clear;
  if eth.eth_compileSolidity(MemoSoliditySource.Lines.Text, s) then
    begin
      MemoABICustom.Lines.Text := s;
      MemoSolidityResult.Lines.Text := s;

      TabControlSolidity.ActiveTab := TabItemSolidityResult;
      TabControlABI.ActiveTab := TabItemABICustom;
    end;
end;

procedure TformMain.ButtonProcessABIClick(Sender: TObject);
var
  s: String;
  pa: Boolean;
  c: TEthereumContract;
begin

{  case TabControlABI.ActiveTab.Index of
    0: eth.ABIToContract('DemoContract', MemoABIFull.Text, MemoABIResult.Lines, 'DemoContract.pas');
    1: eth.ABIToContract('DemoContract', MemoABIShort.Text, MemoABIResult.Lines, 'DemoContract.pas');
    2: eth.ABIToContract('DemoContract', MemoABICustom.Text, MemoABIResult.Lines, 'DemoContract.pas');
  end;}

  c := TEthereumContract.Create;
  try
    case TabControlABI.ActiveTab.Index of
      0: pa := c.ParseABI(MemoABIFull.Text);
      1: pa := c.ParseABI(MemoABIShort.Text);
      2: pa := c.ParseABI(MemoABICustom.Text);
    end;
    if pa then
      begin
        c.BuildDelphiSource('DemoContract', MemoABIDelphiSource.Lines);
        if c.BuildABI(s, True) then
          begin
            MemoContractABI.Lines.Text := s;
            EditContractAddress.Text := c.ContractAddress;
          end;
      end;
  finally
    c.Free;
  end;

  TabControlABI.ActiveTab := TabItemABIResult;
end;

procedure TformMain.ButtonSaveDelphiClick(Sender: TObject);
begin
  MemoABIDelphiSource.Lines.SaveToFile('DemoContract.pas');
end;

procedure TformMain.ButtonStartMinerClick(Sender: TObject);
var
  Result: Boolean;
begin
  eth.miner_start(1, Result);
end;

procedure TformMain.ButtonStopMinerClick(Sender: TObject);
var
  Result: Boolean;
begin
  eth.miner_stop(Result);
end;

procedure TformMain.CallCodeButtonClick(Sender: TObject);
var
  c: TEthereumContract;
begin
  c := TEthereumContract.Create;
  try
    //
  finally
    c.Free;
  end;
end;

procedure TformMain.ComboBoxFromAddressClick(Sender: TObject);
var
  Account: String;
  Accounts: TArray<String>;
begin
  if (ComboBoxFromAddress.Items.Count = 0) and eth.eth_accounts(Accounts) then
    begin
      ComboBoxFromAddress.Items.Clear;
      for Account in Accounts do
        ComboBoxFromAddress.Items.Add(Account);
    end;
end;

procedure TformMain.EditServerChange(Sender: TObject);
begin
  SetupEthereum(eth);
  SetupEthereum(demo);
end;

procedure TformMain.FormCreate(Sender: TObject);
begin
  FEth := TEthereum.Create;
  demo := TEth_ContractDemoContract.Create;
  FWatch := TEth_ContractDemoContract.Create;

  Eth := FEth;
  EditServerChange(nil);
end;

procedure TformMain.FormDestroy(Sender: TObject);
begin
  Eth.Free;
  demo.Free;
  FWatch.Free;
end;

procedure TformMain.FormShow(Sender: TObject);
begin
  MemoWatchABIChange(nil);
end;

procedure TformMain.MemoWatchABIChange(Sender: TObject);
begin
  SetupEthereum(FWatch);
  FWatch.ContractAddress := EditContractAddress.Text;
  FWatch.Methods.Clear;
  FWatch.Events.Clear;
  if FWatch.ParseABI(MemoWatchABI.Text) then
    begin
      FWatch.ContractAddress := EditWatchAddress.Text;
      StringGridEvents.RowCount := 0;
      StringGridState.RowCount := 0;
      StringGridMethods.RowCount := 0;
      SetupEthereum(FWatch);
    end;
end;

procedure TformMain.OnMethodCall(Sender: TObject);
begin
  AddLog('Call', (Sender as TEthereum).MethodCall.ToJSON);
end;

procedure TformMain.OnMethodError(Sender: TObject);
begin
  LabelError.Text := (Sender as TEthereum).MethodError.code.ToString + ': ' + (Sender as TEthereum).MethodError.message;
  AddLog('Error', LabelError.Text);
end;

procedure TformMain.OnMethodResult(Sender: TObject);
begin
  AddLog('Result', (Sender as TEthereum).MethodResult.ToJSON);
end;

procedure TformMain.SetupEthereum(Eth: TEthereum);
begin
  Eth.RpcAddress := EditServer.Text;
  Eth.RpcPort := Trunc(NumberBoxPort.Value);

  if ComboBoxFromAddress.ItemIndex <> - 1 then
    Eth.CoinAddress := ComboBoxFromAddress.Items[ComboBoxFromAddress.ItemIndex];
  Eth.CoinPassword := EditFromPassword.Text;

  Eth.OnMethodCall := OnMethodCall;
  Eth.OnMethodResult := OnMethodResult;
  Eth.OnMethodError := OnMethodError;
end;

procedure TformMain.SpeedButton1Click(Sender: TObject);
var
  Tab: TTabItem;
  formTr: TformTransaction;
begin
  Tab := TabControl1.Add;
  Tab.Text := TformTransaction.ClassName;

  formTr := TformTransaction.Create(Tab);
  formTr.Align := TAlignLayout.Client;
  formTr.Parent := Tab;
end;

procedure TformMain.SpeedButton2Click(Sender: TObject);
var
  Tab: TTabItem;
  form: TformBlock;
begin
  Tab := TabControl1.Add;
  Tab.Text := TformBlock.ClassName;

  form := TformBlock.Create(Tab);
  form.Align := TAlignLayout.Client;
  form.Parent := Tab;
end;

procedure TformMain.StringGridEventsCellClick(const Column: TColumn;
  const Row: Integer);
var
  i, j: Integer;
  b: Boolean;
  s: String;
  EventHash: String;
  bytes32: TByteDynArray;
  Topics: TArray<String>;
  Event: TEthereumContractEvent;
  Events: TObjectList<TEth_FilterChangeClass>;
begin
  Events := nil;
  Event := FWatch.Event[StringGridEvents.Cells[0, Row]];
  try
    if Assigned(Event) and FWatch.GetEventHash(Event) then
      begin
        StringGridEvents.Cells[1, Row] := Event.EventHash;
        StringGridEvents.Cells[2, Row] := Event.EthereumEventSignature;

        SetLength(Topics, 1);
        Topics[0] := Event.EventHash;
        if FWatch.eth_getLogs(ethbnEearliest, 0, ethbnLatest, 0, FWatch.ContractAddress, Topics, Events) then
          begin
            for i := 0 to Events.Count - 1 do
              s := s + Format('%s(%d)', [Events[i].blockNumber, j]);
          end else
            s := FWatch.MethodError.message;

        StringGridEvents.Cells[3, Row] := s;
      end;
  finally
    Events.Free;
  end;
end;

procedure TformMain.TabMethodsClick(Sender: TObject);
var
  i: Integer;
begin
  if StringGridMethods.RowCount = 0 then
    for i := 0 to FWatch.Methods.Count - 1 do
      begin
        StringGridMethods.RowCount := StringGridMethods.RowCount + 1;
        StringGridMethods.Cells[0, StringGridMethods.RowCount - 1] := FWatch.Methods[i].EthereumMethodSignature;
        StringGridMethods.Cells[1, StringGridMethods.RowCount - 1] := FWatch.Methods[i].MethodType;
        StringGridMethods.Cells[2, StringGridMethods.RowCount - 1] := BoolToStr(FWatch.Methods[i].MethodPayable, True);
        StringGridMethods.Cells[3, StringGridMethods.RowCount - 1] := BoolToStr(FWatch.Methods[i].MethodConstant, True);
        StringGridMethods.Cells[4, StringGridMethods.RowCount - 1] := '***';
      end;
end;

procedure TformMain.TabStateClick(Sender: TObject);
var
  i, j: Integer;
  Code, CallResult: String;
  Method: TEthereumContractMethod;
begin
  if StringGridState.RowCount = 0 then
    for i := 0 to FWatch.Methods.Count - 1 do
      begin
        Method := FWatch.Methods[i];
        if Method.MethodConstant and (Method.Outputs.Count > 0) then
          begin
            StringGridState.RowCount := StringGridState.RowCount + 1;
            StringGridState.Cells[0, StringGridState.RowCount - 1] := Method.MethodName;
            StringGridState.Cells[1, StringGridState.RowCount - 1] := Method.MethodType;
            if Demo.BuildCallCode(Method, [], Code)
            and Demo.eth_call(Demo.CoinAddress, Demo.ContractAddress, 100000, 100000, 0, Code, ethbnLatest, 0, CallResult) then
              begin
                if (Method.Outputs.Count > 0)
                and Demo.ParseCallCode(ClassName, CallResult, Method.Outputs) then
                  begin
                    Code := '';
                    for j := 0 to Method.Outputs.Count - 1 do
                      Code := Code + Format('%d: %s: %s: %s', [j, Method.Outputs[j].Name, Method.Outputs[j].&Type, Method.Outputs[j].Value]) + sLineBreak;
                    StringGridState.Cells[2, StringGridState.RowCount - 1] := Code;
                  end
                else StringGridState.Cells[2, StringGridState.RowCount - 1] := Demo.MethodError.message;
              end
            else
              StringGridState.Cells[2, StringGridState.RowCount - 1] := Demo.MethodError.message;
          end;
      end;
end;

procedure TformMain.TabEventsClick(Sender: TObject);
var
  i: Integer;
begin
  if StringGridEvents.RowCount = 0 then
    for i := 0 to FWatch.Events.Count - 1 do
      begin
        StringGridEvents.RowCount := StringGridEvents.RowCount + 1;
        StringGridEvents.Cells[0, StringGridEvents.RowCount - 1] := FWatch.Events[i].EventName;
      end;
end;

procedure TformMain.TimerHashRateTimer(Sender: TObject);
var
  Version: String;
  HashRate: Int64;
begin
  if CheckBoxHashRate.IsChecked
  and eth.web3_clientVersion(Version) and eth.eth_hashrate(HashRate) then
      Caption := Format('%s - %.2f Kh/s', [Version, HashRate/1000]);
end;

end.



{

procedure TForm3.ButtonContractClick(Sender: TObject);
var
  s: String;
  b: Boolean;
  eth: TEthContractDemo;
  txs: TTxpool_statusClass;
  ani: TAdmin_nodeInfoClass;
  ape: TAdmin_nodePeersClass;
begin
  eth := TEthContractDemo.Create;
  eth.OnMethodCall := OnMethodCall;
  eth.OnMethodResult := OnMethodResult;
  eth.OnMethodError := OnMethodError;

  eth.RpcAddress := EditServer.Text;
  eth.RpcPort := Trunc(NumberBoxPort.Value);

  eth.admin_peers(ape);
  exit;

  eth.admin_nodeInfo(ani);
  exit;

  eth.txpool_inspect(s);
  exit;
  eth.personal_newAccount('hello', s);
  eth.addPhoto('firstName', 'lastName', 'patronymicName', '1020', 'fullPhoto', 'thumbnailPhoto'+DateTimeToStr(Now));
  eth.Free;
end;

}
