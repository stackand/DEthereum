unit frmTransaction;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.ScrollBox, FMX.Edit,
  FMX.Controls.Presentation, FMX.TabControl,
  DEthereum, DEthereum.Types;

type
  TformTransaction = class(TFrame)
    EditSearch: TEdit;
    SearchButton: TSearchEditButton;
    StringGridTransation: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    procedure SearchButtonClick(Sender: TObject);
    procedure StringGridTransationCellDblClick(const Column: TColumn;
      const Row: Integer);
  private
    { Private declarations }
    function TransactionInfo(Tr: TEth_TransactionClass): TArray<String>;
  public
    { Public declarations }
  end;

implementation

uses
  frmMain,
  frmBlock;

{$R *.fmx}

procedure TformTransaction.StringGridTransationCellDblClick(
  const Column: TColumn; const Row: Integer);
begin
  if SameText(Column.Header, 'BlockHash') then
    NewTabBlock(StringGridTransation.Cells[Column.Index, StringGridTransation.Row]) else
  if SameText(Column.Header, 'TransactionHash') then
    NewTabTransaction(StringGridTransation.Cells[Column.Index, StringGridTransation.Row]);
end;

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
