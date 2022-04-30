PROGRAM Chess(INPUT, OUTPUT);
CONST
  MAX_FIELD_SIZE = 8;                                                                                                                                                                                                               
  QUEEN_SYMBOL = 'Q';
  BISHOP_SYMBOL = 'B';
  KNIGHT_SYMBOL = 'K';
  UNDER_ATTACK_SYMBOL = '.';
  VOID_SYMBOL = '#';

TYPE
  FieldState = (UnderAttack, Void, Knight, Bishop, Queen);
  Field = ARRAY[1 .. MAX_FIELD_SIZE, 1 .. MAX_FIELD_SIZE] OF FieldState;                                                        

VAR
  ChessField: Field;
                         
FUNCTION ConvertLetterToDigit(Letter: CHAR): INTEGER;
VAR
  ConvertedValue: INTEGER;
BEGIN
  ConvertedValue := BYTE(Letter) - BYTE('a') + 1;
  ConvertLetterToDigit := ConvertedValue
END;

FUNCTION ConvertFieldStateToSymbol(State: FieldState): CHAR;
VAR
  Symbol: CHAR;
BEGIN
  CASE State OF
    Queen: Symbol := QUEEN_SYMBOL;
    Bishop: Symbol := BISHOP_SYMBOL;
    Knight: Symbol := KNIGHT_SYMBOL;
    UnderAttack: Symbol := UNDER_ATTACK_SYMBOL;   
    Void: Symbol := VOID_SYMBOL;
  END;  
  
  ConvertFieldStateToSymbol := Symbol;
END;


FUNCTION ConvertSymbolToFieldState(Letter: CHAR): FieldState;
VAR
  State: FieldState;
BEGIN
  CASE Letter OF
    QUEEN_SYMBOL: State := Queen;
    BISHOP_SYMBOL: State := Bishop;
    KNIGHT_SYMBOL: State := Knight;
  ELSE
    State := Void
  END;  
  
  ConvertSymbolToFieldState := State;
END;

PROCEDURE FillChessField(VAR ChessField: Field);
VAR
  IndexI, IndexJ: INTEGER;
  ColumnIndex, RowIndex: INTEGER;
  Letter: CHAR;
  Value: CHAR;
  State: FieldState;
BEGIN
  WHILE NOT EOF
  DO
    BEGIN
      READ(Letter);
      READ(RowIndex);
      READ(Value);
      READ(Value);
                
      ColumnIndex := ConvertLetterToDigit(Letter);
      State := ConvertSymbolToFieldState(Value);
      ChessField[MAX_FIELD_SIZE - RowIndex + 1][ColumnIndex] := State;

      READLN
    END
END;

PROCEDURE InitChessField(VAR ChessField: Field);
VAR
  IndexI, IndexJ: INTEGER;
BEGIN
  FOR IndexI := 1 TO MAX_FIELD_SIZE
  DO
    FOR IndexJ := 1 TO MAX_FIELD_SIZE
    DO
      ChessField[IndexI][IndexJ] := Void
END;

PROCEDURE WriteField(VAR ChessField: Field);
VAR
  IndexI, IndexJ: INTEGER;
BEGIN                                
  FOR IndexI := 1 TO MAX_FIELD_SIZE
  DO
    BEGIN
      FOR IndexJ := 1 TO MAX_FIELD_SIZE
      DO
        WRITE(ConvertFieldStateToSymbol(ChessField[IndexI][IndexJ]));
      WRITELN
    END
END;

BEGIN
  InitChessField(ChessField);
  FillChessField(ChessField);
  WriteField(ChessField) 
END.
