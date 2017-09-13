program Test;

uses
  System.StartUpCopy,
  FMX.Forms,
  frmTest in 'frmTest.pas' {Form3},
  DEthereum.Types in 'DEthereum.Types.pas'
    ;
//  ,DemoContract;


{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
