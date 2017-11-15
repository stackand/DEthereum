unit frmTransaction;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.ScrollBox, FMX.Edit,
  FMX.Controls.Presentation,
  DEthereum, DEthereum.Types;

type
  TformTransaction = class(TFrame)
    EditSearch: TEdit;
    SearchButton: TSearchEditButton;
    StringGridTransation: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    procedure SearchButtonClick(Sender: TObject);
  private
    { Private declarations }
    function TransactionInfo(Tr: TEth_TransactionClass): TArray<String>;
  public
    { Public declarations }
  end;

implementation

uses
  frmMain;

{$R *.fmx}

function TformTransaction.TransactionInfo(Tr: TEth_TransactionClass): TArray<String>;
begin
  Result := [
    'blockHash',          Tr.blockHash,
    'blockNumber',        Tr.blockNumber,
    'from',               Tr.from,
    'gas',                Tr.gas,
    'gasPrice',           Tr.gasPrice,
    'hash',               Tr.hash,
    'input',              Tr.input,
    'nonce',              Tr.nonce,
    'r',                  Tr.r,
    's',                  Tr.s,
    'transactionIndex',   Tr.transactionIndex,
    'v',                  Tr.v,
    'value',              Tr.value
  ];
end;

procedure TformTransaction.SearchButtonClick(Sender: TObject);
var
  Tr: TEth_TransactionClass;
begin
  //SetTitle(EditTransaction.Text);
  Tr := nil;
  try
    if Eth.eth_getTransactionByHash(EditSearch.Text, Tr) and Assigned(Tr) then
      InfoToStringGrid(TransactionInfo(Tr), StringGridTransation, True);
  finally
    FreeAndNil(Tr);
  end;
end;

end.
