unit DEthereum.BigInt;

interface

uses
  SysUtils, Math;

type
  TDigitArray = array of Byte;


  TBigInte = class
    Negative: Boolean;
    Digit: TDigitArray;
    constructor Create(s: String = '0');
    procedure AbsAdd(const b: TBigInte);
    procedure Add(const v: TBigInte);
    procedure Multiply(const v: TBigInte);
    procedure MultiplyByRadix(const Power: Integer);
    procedure Subtract(const v: TBigInte);
    function AbsCompare(const v: TBigInte): Integer;
    function ToString: String;
  end;

  TBigInt = record
    Digit: TDigitArray;
    Negative: Boolean;
  end;

function BigIntAbsAdd(const a, b: TBigInt): TBigInt;
function BigIntAdd(const a, b: TBigInt): TBigInt;
function BigIntMultiply(const a, b: TBigInt): TBigInt;
function BigIntMultiplyByRadix(const b: TBigInt; const Power: Integer): TBigInt;
function BigIntSubtract(const a: TBigInt; b: TBigInt): TBigInt;
function BigIntAbsCompare(const a, b: TBigInt): Integer;
function StrToBigInt(s: String): TBigInt;
function BigIntToStr(const b: TBigInt): String;

implementation

type
  TOpCell = record
    Digit, Carry: Byte;
  end;
  TOpTable = Array[0..1, 0..9, 0..9] of TOpCell;

var
  AddTable, SubtractTable: TOpTable;

function BigIntToStr(const b: TBigInt): String;
var
  j: Integer;
begin
  SetLength(Result, Length(b.Digit));
  for j := 0 to Length(b.Digit) - 1 do
   Result[Length(Result) - j] := Chr(b.Digit[j] + 48);

  if b.Negative then Result := '-' + Result;
end;

function StrToBigInt(s: String): TBigInt;
var
  j: Integer;
begin
  // Assuming valid input!
  // Check for the Minus sign
  if s[1] = '-' then
    begin
     Result.Negative := True;
     Delete(s, 1, 1);
    end else
  Result.Negative := False;

  // Read the digits
  SetLength(Result.Digit, Length(s));
  for j := 1 to Length(s) do
    Result.Digit[Length(s) - j] := Ord(s[j]) - 48;
end;

// Return 1 if a > b, 0 if a = b and -1 if a < b
function BigIntAbsCompare(const a, b: TBigInt): Integer;
var
  j: Integer;
begin
  if Length(a.Digit) > Length(b.Digit) then Result := 1 else
  if Length(a.Digit) < Length(b.Digit) then Result := -1 else
    begin
      // Lengths are the same - compare digit by digit
      Result := 0;
      for j := Length(a.Digit) - 1 downto 0 do
      if a.Digit[j] <> b.Digit[j] then
        begin
          if a.Digit[j] > b.Digit[j] then Result := 1 else Result := -1;
          Break;
        end;
    end;
end;

// Return digit at index, or 0 if index is over high bound
function BigIntDigit(const b: TBigInt; const Index: Integer): Byte;
begin
  if Index < Length(b.Digit) then Result := b.Digit[Index] else Result := 0;
end;

// Return |a| - |b|, assuming |a| > |b|
function BigIntAbsSubtract(const a, b: TBigInt): TBigInt;
var
  j, MaxDigitIndex: Integer;
  SubCell: TOpCell;
begin
  Result.Negative := False;
  MaxDigitIndex := Max(Length(a.Digit), Length(b.Digit)) - 1;
  SetLength(Result.Digit, MaxDigitIndex + 1);
  SubCell.Carry := 0;

  for j := 0 to MaxDigitIndex do
   begin
     SubCell := SubtractTable[SubCell.Carry, BigIntDigit(a, j), BigIntDigit(b, j)];
     Result.Digit[j] := SubCell.Digit;
   end;

  // Remove extra zeros (e.g. for 100 - 99, trim 001 into 1)
  j := MaxDigitIndex;
  while Result.Digit[j] = 0 do Dec(j);
  SetLength(Result.Digit, j + 1);
end;

function BigIntSubtract(const a: TBigInt; b: TBigInt): TBigInt;
begin
  if b.Negative then
  begin
     // b < 0, so this is the same as a + |b|;
    b.Negative := False;
    Result := BigIntAdd(a, b);
  end else
  begin
    // b >= 0
    if a.Negative then
     begin
       // a < 0, so this is like -(|a| + |b|)
       Result := BigIntAbsAdd(a, b);
       Result.Negative := True;
     end else
      begin
        // Both numbers are positive
        case BigIntAbsCompare(a, b) of
          -1: begin
                 // b > a, so a - b = -(b - a)
                 Result := BigIntAbsSubtract(b, a);
                 Result.Negative := True;
               end;
           0: Result := StrToBigInt('0');
           1: Result := BigIntAbsSubtract(a, b);
        end;
      end;
    end;
end;

// Return |a| + |b|
function BigIntAbsAdd(const a, b: TBigInt): TBigInt;
var
  j, MaxDigitIndex: Integer;
  AddCell: TOpCell;
begin
  // Make room in result
  Result.Negative := False;
  MaxDigitIndex := Max(Length(a.Digit), Length(b.Digit)) - 1;
  SetLength(Result.Digit, MaxDigitIndex + 1);
  AddCell.Carry := 0;

  // Add actual digits
  for j := 0 to MaxDigitIndex do
    begin
      AddCell := AddTable[AddCell.Carry, BigIntDigit(a, j), BigIntDigit(b, j)];
      Result.Digit[j] := AddCell.Digit;
    end;

  // Add extra "1" if there's a carry left
  if AddCell.Carry = 1 then
   begin
     SetLength(Result.Digit, Length(Result.Digit) + 1);
     Result.Digit[Length(Result.Digit) - 1] := 1;
   end;
end;

function BigIntAdd(const a, b: TBigInt): TBigInt;
var
  CompareResult: ShortInt;
begin
  if a.Negative = b.Negative then
    begin
      // Remember -x + -y = -(x + y)
      Result := BigIntAbsAdd(a, b);
      Result.Negative := a.Negative;
    end else
     begin
       // We know one of the numbers (and only one) is negative...
       CompareResult := BigIntAbsCompare(a, b);
       case CompareResult of
         -1: begin
                // |a| < |b|, so b determines the result sign
                Result := BigIntAbsSubtract(b, a);
                Result.Negative := b.Negative;
              end;
          0: Result := StrToBigInt('0');
          1: begin
                // |a| > |b|, so a determines the result sign
                Result := BigIntAbsSubtract(a, b);
                Result.Negative := a.Negative;
              end;
       end;
     end;
end;

// Return the number multiplied by 10^Power (by adding zeros)
function BigIntMultiplyByRadix(const b: TBigInt; const Power: Integer): TBigInt;
begin
  // If the number is 0, there's nothing to multiply
  if BigIntToStr(b) <> '0' then
    begin
      // Sign remains the same
      Result.Negative := b.Negative;
      // Make space in result for new zeros + b's digits
      SetLength(Result.Digit, Length(b.Digit) + Power);
      // Fill in zeros, then copy digits
      FillChar(Result.Digit[0], Power, #0);
      Move(b.Digit[0], Result.Digit[Power], Length(b.Digit));
    end else
  Result := b;
end;

function BigIntMultiply(const a, b: TBigInt): TBigInt;
var
  i, j: Integer;
  MulTable: array[1..9] of TBigInt;
begin
  Result := StrToBigInt('0');

  // Create reference table of a*1, a*2, ..., a*9
  for i := Low(MulTable) to High(MulTable) do MulTable[i] := StrToBigInt('0');
  for i := Low(MulTable) to High(MulTable) do
  for j := i to High(MulTable) do
    MulTable[j] := BigIntAbsAdd(MulTable[j], a);

  // Sum up single-digit products
  for i := 0 to Length(b.Digit) - 1 do
    if b.Digit[i] <> 0 then
      Result := BigIntAbsAdd(Result, BigIntMultiplyByRadix(MulTable[b.Digit[i]], i));

  // Minus times Plus is Minus, all other combinations are Plus
  Result.Negative := a.Negative <> b.Negative;
end;

procedure FillOpTables;
var
  a, b, c, n: Integer;
begin
  for c := 0 to 1 do
    for a := 0 to 9 do
      for b := 0 to 9 do
        begin
          n := a + b + c;
          AddTable[c, a, b].Carry := n div 10;
          AddTable[c, a, b].Digit := n mod 10;
          n := a - b - c;
          if n >= 0 then
            begin
              SubtractTable[c, a, b].Carry := 0;
              SubtractTable[c, a, b].Digit := n;
            end else
          begin
            SubtractTable[c, a, b].Carry := 1;
            SubtractTable[c, a, b].Digit := 10 + n;
          end;
        end;
end;

function RandomBigInt(const Digits: Integer): TBigInt;
var
  s: String;
  j: Integer;
begin
  SetLength(s, Digits);
  for j := 1 to Digits do
    s[j] := Chr(Random(10) + 48);
  while (Length(s) > 1) AND (s[1] = '0') do Delete(s, 1, 1);

  if Random(2) = 0 then s := '-' + s;
  Result := StrToBigInt(s);
end;

procedure Main;
var
  a, b: TBigInt;
  Digits, e: Integer;
  s: String;
begin
  randomize;
//  FillOpTables;

  Digits := 1;
  repeat
    a := RandomBigInt(Digits);
    b := RandomBigInt(Digits);
    Writeln('a     = ', BigIntToStr(a));
    Writeln('b     = ', BigIntToStr(b));
    Writeln('a + b = ', BigIntToStr(BigIntAdd(a, b)));
    Writeln('a - b = ', BigIntToStr(BigIntSubtract(a, b)));
    Writeln('a * b = ', BigIntToStr(BigIntMultiply(a, b)));
    Writeln;
    Write('Set max number of digits [ENTER=', Digits, '], 0 or letters to exit: ');
    Readln(s);
    if s <> '' then Val(s, Digits, e);

  until (e <> 0) OR (Digits <= 0);
end;


{ TBigInte }

procedure TBigInte.AbsAdd(const v: TBigInte);
var
  j, MaxDigitIndex: Integer;
  AddCell: TOpCell;
  a: TBigInte;
begin
  // Make room in result

  a := Self;
  Result.Negative := False;
  MaxDigitIndex := Max(Length(a.Digit), Length(b.Digit)) - 1;
  SetLength(Result.Digit, MaxDigitIndex + 1);
  AddCell.Carry := 0;

  // Add actual digits
  for j := 0 to MaxDigitIndex do
    begin
      AddCell := AddTable[AddCell.Carry, BigIntDigit(a, j), BigIntDigit(b, j)];
      Result.Digit[j] := AddCell.Digit;
    end;

  // Add extra "1" if there's a carry left
  if AddCell.Carry = 1 then
   begin
     SetLength(Result.Digit, Length(Result.Digit) + 1);
     Result.Digit[Length(Result.Digit) - 1] := 1;
   end;
end;

function TBigInte.AbsCompare(const v: TBigInte): Integer;
begin

end;

procedure TBigInte.Add(const v: TBigInte);
begin

end;

constructor TBigInte.Create(s: String);
var
  j: Integer;
begin
  inherited;
  // Assuming valid input!
  // Check for the Minus sign
  if s[1] = '-' then
    begin
     Negative := True;
     Delete(s, 1, 1);
    end else
  Negative := False;

  // Read the digits
  SetLength(Digit, Length(s));
  for j := 1 to Length(s) do
    Digit[Length(s) - j] := Ord(s[j]) - 48;
end;

procedure TBigInte.Multiply(const v: TBigInte);
begin

end;

procedure TBigInte.MultiplyByRadix(const Power: Integer);
begin

end;

procedure TBigInte.Subtract(const v: TBigInte);
begin

end;

function TBigInte.ToString: String;
var
  j: Integer;
begin
  SetLength(Result, Length(Digit));
  for j := 0 to Length(Digit) - 1 do
   Result[Length(Result) - j] := Chr(Digit[j] + 48);

  if Negative then Result := '-' + Result;
end;

initialization
  FillOpTables;
end.
