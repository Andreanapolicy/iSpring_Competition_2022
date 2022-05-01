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
    Void: Symbol := VOID_SYMBOL
  END;  
  
  ConvertFieldStateToSymbol := Symbol
END;


FUNCTION ConvertSymbolToFieldState(Letter: CHAR): FieldState;
VAR
  State: FieldState;
BEGIN
  CASE Letter OF
    QUEEN_SYMBOL: State := Queen;
    BISHOP_SYMBOL: State := Bishop;
    KNIGHT_SYMBOL: State := Knight
  ELSE
    State := Void
  END;  
  
  ConvertSymbolToFieldState := State
END;

FUNCTION IsFigure(State: FieldState): BOOLEAN;
BEGIN
  IsFigure := (State = Queen) OR (State = Bishop) OR (State = Knight)
END;
  
PROCEDURE ProcessHorizontalLine(VAR ChessField: Field; IndexI, IndexJ: INTEGER);
VAR
  Step: INTEGER;
BEGIN  
  FOR Step := 1 TO MAX_FIELD_SIZE
  DO
    BEGIN
      IF (IndexI + Step <= MAX_FIELD_SIZE) AND IsFigure(ChessField[IndexI + Step][IndexJ])
      THEN
        BREAK;
        
      IF (IndexI + Step <= MAX_FIELD_SIZE) AND (ChessField[IndexI + Step][IndexJ] = Void)
      THEN
        ChessField[IndexI + Step][IndexJ] := UnderAttack
    END;   
    
  FOR Step := 1 TO MAX_FIELD_SIZE
  DO
    BEGIN
      IF (IndexI - Step >= 1) AND IsFigure(ChessField[IndexI - Step][IndexJ])
      THEN
        BREAK;
        
      IF (IndexI - Step >= 1) AND (ChessField[IndexI - Step][IndexJ] = Void)
      THEN
        ChessField[IndexI - Step][IndexJ] := UnderAttack
    END
END;

PROCEDURE ProcessVerticalLine(VAR ChessField: Field; IndexI, IndexJ: INTEGER);
VAR
  Step: INTEGER;
BEGIN  
  FOR Step := 1 TO MAX_FIELD_SIZE
  DO
    BEGIN
      IF (IndexJ + Step <= MAX_FIELD_SIZE) AND IsFigure(ChessField[IndexI][IndexJ + Step])
      THEN
        BREAK;
        
      IF (IndexJ + Step <= MAX_FIELD_SIZE) AND (ChessField[IndexI][IndexJ + Step] = Void)
      THEN
        ChessField[IndexI][IndexJ + Step] := UnderAttack
    END;
  
  FOR Step := 1 TO MAX_FIELD_SIZE
  DO
    BEGIN
      IF (IndexJ - Step >= 1) AND IsFigure(ChessField[IndexI][IndexJ - Step])
      THEN
        BREAK;
        
      IF (IndexJ - Step >= 1) AND (ChessField[IndexI][IndexJ - Step] = Void)
      THEN
        ChessField[IndexI][IndexJ - Step] := UnderAttack
    END
END;

PROCEDURE ProcessDiagonals(VAR ChessField: Field; IndexI, IndexJ: INTEGER);
VAR
  Step: INTEGER;
BEGIN
  FOR Step := 1 TO MAX_FIELD_SIZE
  DO
    BEGIN
      IF (IndexI - Step >= 1) AND (IndexJ + Step <= MAX_FIELD_SIZE) AND IsFigure(ChessField[IndexI  - Step][IndexJ + Step])
      THEN
        BREAK;
        
      IF (IndexI - Step >= 1) AND (IndexJ + Step <= MAX_FIELD_SIZE) AND (ChessField[IndexI - Step][IndexJ + Step] = Void)
      THEN
        ChessField[IndexI - Step][IndexJ + Step] := UnderAttack
    END;
    
  FOR Step := 1 TO MAX_FIELD_SIZE
  DO
    BEGIN
      IF (IndexI + Step <= MAX_FIELD_SIZE) AND (IndexJ + Step <= MAX_FIELD_SIZE) AND IsFigure(ChessField[IndexI + Step][IndexJ + Step])
      THEN
        BREAK;
        
      IF (IndexI + Step <= MAX_FIELD_SIZE) AND (IndexJ + Step <= MAX_FIELD_SIZE) AND (ChessField[IndexI + Step][IndexJ + Step] = Void)
      THEN
        ChessField[IndexI + Step][IndexJ + Step] := UnderAttack
    END;
        
  FOR Step := 1 TO MAX_FIELD_SIZE
  DO
    BEGIN
      IF (IndexI + Step <= MAX_FIELD_SIZE) AND (IndexJ - Step >= 1) AND IsFigure(ChessField[IndexI + Step][IndexJ - Step])
      THEN
        BREAK;
        
      IF (IndexI + Step <= MAX_FIELD_SIZE) AND (IndexJ - Step >= 1) AND (ChessField[IndexI + Step][IndexJ - Step] = Void)
      THEN
        ChessField[IndexI + Step][IndexJ - Step] := UnderAttack
    END;
   
  FOR Step := 1 TO MAX_FIELD_SIZE
  DO
    BEGIN
      IF (IndexI - Step >= 1) AND (IndexJ - Step >= 1) AND IsFigure(ChessField[IndexI - Step][IndexJ - Step])
      THEN
        BREAK;
        
      IF (IndexI - Step >= 1) AND (IndexJ - Step >= 1) AND (ChessField[IndexI - Step][IndexJ - Step] = Void)
      THEN
        ChessField[IndexI - Step][IndexJ - Step] := UnderAttack
    END    
END;    
   
PROCEDURE PaintOverChessFieldByQueenAttack(VAR ChessField: Field; IndexI, IndexJ: INTEGER);
VAR
  Step: INTEGER;
BEGIN
  ProcessDiagonals(ChessField, IndexI, IndexJ);
  ProcessHorizontalLine(ChessField, IndexI, IndexJ);
  ProcessVerticalLine(ChessField, IndexI, IndexJ)
END;

PROCEDURE PaintOverChessFieldByBishopAttack(VAR ChessField: Field; IndexI, IndexJ: INTEGER);
VAR
  Step: INTEGER;
BEGIN
  ProcessDiagonals(ChessField, IndexI, IndexJ)
END;

PROCEDURE PaintOverChessFieldByKnightAttack(VAR ChessField: Field; IndexI, IndexJ: INTEGER);
BEGIN    
  IF (IndexI - 1 >= 1) AND (IndexJ - 2 >= 1) AND (ChessField[IndexI - 1][IndexJ - 2] = Void)
  THEN
    ChessField[IndexI - 1][IndexJ - 2] := UnderAttack;
        
  IF (IndexI - 2 >= 1) AND (IndexJ - 1 >= 1) AND (ChessField[IndexI - 2][IndexJ - 1] = Void)
  THEN
    ChessField[IndexI - 2][IndexJ - 1] := UnderAttack;
        
  IF (IndexI + 2 <= MAX_FIELD_SIZE) AND (IndexJ - 1 >= 1) AND (ChessField[IndexI + 2][IndexJ - 1] = Void)
  THEN
    ChessField[IndexI + 2][IndexJ - 1] := UnderAttack;
        
  IF (IndexI + 1 <= MAX_FIELD_SIZE) AND (IndexJ - 2 >= 1) AND (ChessField[IndexI + 1][IndexJ - 2] = Void)
  THEN
    ChessField[IndexI + 1][IndexJ - 2] := UnderAttack;
        
  IF (IndexI + 1 <= MAX_FIELD_SIZE) AND (IndexJ + 2 <= MAX_FIELD_SIZE) AND (ChessField[IndexI + 1][IndexJ + 2] = Void)
  THEN
    ChessField[IndexI + 1][IndexJ + 2] := UnderAttack;
        
  IF (IndexI + 2 <= MAX_FIELD_SIZE) AND (IndexJ + 1 <= MAX_FIELD_SIZE) AND (ChessField[IndexI + 2][IndexJ + 1] = Void)
  THEN
    ChessField[IndexI + 2][IndexJ + 1] := UnderAttack;
        
  IF (IndexI - 1 >= 1) AND (IndexJ + 2 <= MAX_FIELD_SIZE) AND (ChessField[IndexI - 1][IndexJ + 2] = Void)
  THEN
    ChessField[IndexI - 1][IndexJ + 2] := UnderAttack;
        
  IF (IndexI - 2 >= 1) AND (IndexJ + 1 <= MAX_FIELD_SIZE) AND (ChessField[IndexI - 2][IndexJ + 1] = Void)
  THEN
    ChessField[IndexI - 2][IndexJ + 1] := UnderAttack
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

PROCEDURE PaintOverChessField(VAR ChessField: Field);
VAR
  IndexI, IndexJ: INTEGER;
  Letter: CHAR;
  Value: CHAR;
  State: FieldState;
BEGIN
  FOR IndexI := 1 TO MAX_FIELD_SIZE
  DO
    BEGIN
      FOR IndexJ := 1 TO MAX_FIELD_SIZE
      DO
        BEGIN
           CASE ChessField[IndexI][IndexJ] OF
             Queen: PaintOverChessFieldByQueenAttack(ChessField, IndexI, IndexJ);
             Bishop: PaintOverChessFieldByBishopAttack(ChessField, IndexI, IndexJ);
             Knight: PaintOverChessFieldByKnightAttack(ChessField, IndexI, IndexJ)
           END  
        END
    END
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
  PaintOverChessField(ChessField);
  WriteField(ChessField) 
END.
