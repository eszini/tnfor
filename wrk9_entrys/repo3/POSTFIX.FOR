C
      SUBROUTINE POSTFIX_NOTATION_OBJECT(OBJECTIVE_FUNCTION_ACTIVE)
C
      use globecom
C
      LOGICAL*1 OBJECTIVE_FUNCTION_ACTIVE
      CHARACTER*80 OBJECTIVE_FUNCTION_DESCRIPTION
      CHARACTER*256 FILE_NAME,
     +              OBJECTIVE_FUNCTION_FILE_NAME,
     +              OBJ_FUNCTION_FILE_NAME
      CHARACTER*512 RECLN
      INTEGER*2 SKIP,I,NUM_OF_VARIABLES,DELETE,
     +          LEFT_BRACKET,RIGHT_BRACKET,ISERROR
      INTEGER IOS
      LOGICAL*4 FILE_EXISTS
      LOGICAL*1 NEW_FORMAT
C
      CHARACTER*80 PARTIAL_OBJFUNC_DES(:),GROUP_STRING(:),
     +             VARIABLE_STRING(:)
      CHARACTER*1 LINKAGE_OPERATOR(:),DISCOUNT_START_PERIOD(:)
      INTEGER*2 VARIABLE_NUM(:),FUNCTION_OPERATOR(:),
     +          START_YEAR(:),END_YEAR(:),
     +          LEFT_BRACKET_COUNT(:),
     +          RIGHT_BRACKET_COUNT(:),
     +          GROUP_NUM(:)
      LOGICAL*1 FIRST_GROUP_OCCURANCE
      INTEGER*2 LAST_GROUP_NUM,POSITION_OF_DIVIDE
      REAL MULTIPLIER(:),DISCOUNT_RATE(:)
      ALLOCATABLE:: PARTIAL_OBJFUNC_DES,GROUP_STRING,
     +              VARIABLE_STRING,
     +              LINKAGE_OPERATOR,DISCOUNT_START_PERIOD,
     +              VARIABLE_NUM,FUNCTION_OPERATOR,
     +              START_YEAR,END_YEAR,
     +              LEFT_BRACKET_COUNT,
     +              RIGHT_BRACKET_COUNT,
     +              GROUP_NUM,
     +              MULTIPLIER,DISCOUNT_RATE
C
      ALLOCATE(PARTIAL_OBJFUNC_DES(50),GROUP_STRING(50),
     +         VARIABLE_STRING(50),
     +         LINKAGE_OPERATOR(50),DISCOUNT_START_PERIOD(50),
     +         VARIABLE_NUM(50),FUNCTION_OPERATOR(50),
     +         START_YEAR(50),END_YEAR(50),
     +         LEFT_BRACKET_COUNT(50),
     +         RIGHT_BRACKET_COUNT(50),
     +         GROUP_NUM(50),
     +         MULTIPLIER(50),DISCOUNT_RATE(50))
C
      OBJECTIVE_FUNCTION_FILE_NAME = OBJ_FUNCTION_FILE_NAME()
      IF(trim(OBJECTIVE_FUNCTION_FILE_NAME) == ' ') THEN
         FILE_EXISTS = .FALSE.
      ELSE
         INQUIRE(FILE=OBJECTIVE_FUNCTION_FILE_NAME,EXIST=FILE_EXISTS)
      ENDIF
c
c this file should be in the project directory LF95's FINDFILE doesn't
c work 9/8/01 msg
c      
c      CALL FINDFILE(OBJECTIVE_FUNCTION_FILE_NAME,FILE_NAME)
c      IOS = ISERROR()
c      IF(IOS == 0) THEN
c         INQUIRE(FILE=FILE_NAME,EXIST=FILE_EXISTS)
c      ELSE
c         FILE_EXISTS = .FALSE.
c      ENDIF
      OBJECTIVE_FUNCTION_ACTIVE = FILE_EXISTS
      IF(OBJECTIVE_FUNCTION_ACTIVE) THEN
         OPEN(10,FILE=OBJECTIVE_FUNCTION_FILE_NAME)
         NEW_FORMAT = INDEX(OBJECTIVE_FUNCTION_FILE_NAME,
     +                                               'OBJFUNC.LST') == 0
         IF(INDEX(OBJECTIVE_FUNCTION_FILE_NAME,'MSGPLSWT.DAT') /= 0)THEN
            READ(10,*,IOSTAT=IOS) DELETE ! READ 9,
            READ(10,*,IOSTAT=IOS) RECLN  ! READ PLANNING SWITCHES                             
         ENDIF   
         IF(NEW_FORMAT) THEN
            READ(10,1000,IOSTAT=IOS) OBJECTIVE_FUNCTION_DESCRIPTION
            READ(10,*,IOSTAT=IOS)         ! SKIP COMMENT LINE
            READ(10,*,IOSTAT=IOS) RECLN   ! EQUATION FORM
         ELSE
            READ(10,*,IOSTAT=IOS) OBJECTIVE_FUNCTION_DESCRIPTION
         ENDIF
C
         I = 0
         IOS = 0
         DO
            READ(10,1000,IOSTAT=IOS) RECLN
            IF(IOS /= 0) THEN
C NEED ERROR MESSAGE
               EXIT
            ENDIF
            POSITION_OF_DIVIDE = INDEX(RECLN,",/,")
            IF(POSITION_OF_DIVIDE /= 0) THEN
               POSITION_OF_DIVIDE = POSITION_OF_DIVIDE  + 1
               RECLN(POSITION_OF_DIVIDE:POSITION_OF_DIVIDE) = 'X'
            ENDIF
            I = I + 1
            LINKAGE_OPERATOR(I)= ' '
            GROUP_STRING(I) = ' '
            READ(RECLN,*,IOSTAT=IOS) PARTIAL_OBJFUNC_DES(I),
     +                 LINKAGE_OPERATOR(I),
     +                 VARIABLE_NUM(I),SKIP,FUNCTION_OPERATOR(I),
     +                 START_YEAR(I),END_YEAR(I),MULTIPLIER(I),
     +                 DISCOUNT_RATE(I),DISCOUNT_START_PERIOD(I),
     +                 LEFT_BRACKET_COUNT(I),RIGHT_BRACKET_COUNT(I),
     +                 GROUP_NUM(I),GROUP_STRING(I),
     +                 VARIABLE_STRING(I)
            IF(IOS /= 0) THEN
               I = 0
               EXIT
            ENDIF
            IF(POSITION_OF_DIVIDE /= 0) LINKAGE_OPERATOR(I) = '/'
         ENDDO
         CLOSE(10)
      ELSE
         OBJECTIVE_FUNCTION_DESCRIPTION = 'PVRR'
         I = 1
         PARTIAL_OBJFUNC_DES(I) = "PVRR = (Present value - Begin)"//
     +                    "(TOTAL OPERATING REVENUES(1999-2024), 9.50%)"
         LINKAGE_OPERATOR(I) = ' '
         VARIABLE_NUM(I) = 30
         FUNCTION_OPERATOR(I) = 4
         START_YEAR(I) = BASE_YEAR + 1
         END_YEAR(I) = LAST_EXTENSION_YEAR
         MULTIPLIER(I) = 1.0
         DISCOUNT_RATE(I) = 9.5
         DISCOUNT_START_PERIOD(I) = 'B'
         LEFT_BRACKET_COUNT(I) = 1
         RIGHT_BRACKET_COUNT(I) = 1
         GROUP_NUM(I) =  0
         GROUP_STRING(I) = ' '
         VARIABLE_STRING(I) =  'PVRR'
      ENDIF
      IF(I > 0) THEN
C
C CREATE THE FIXPOST NOTATION QUE FOR CALCULATING THE OBJECTIVE FUNCTION
C
         NUM_OF_VARIABLES = I
         CALL STORE_OBJECT_FUNCTION_VARIABLES(NUM_OF_VARIABLES,
     +                                   OBJECTIVE_FUNCTION_DESCRIPTION,
     +                                   PARTIAL_OBJFUNC_DES,
     +                                   GROUP_STRING,
     +                                   VARIABLE_STRING,
     +                                   DISCOUNT_START_PERIOD,
     +                                   VARIABLE_NUM,
     +                                   FUNCTION_OPERATOR,
     +                                   START_YEAR,
     +                                   END_YEAR,
     +                                   GROUP_NUM,
     +                                   DISCOUNT_RATE)
         CALL INIT_POSTFIX_FORM
         IF(LINKAGE_OPERATOR(1) /= ' ') THEN
            IF(LINKAGE_OPERATOR(1) == '-')MULTIPLIER(1) = -MULTIPLIER(1) 
            LINKAGE_OPERATOR(1) = ' '
         ENDIF
         LAST_GROUP_NUM = -999
         DO I = 1, NUM_OF_VARIABLES
            CALL SET_OBJ_LINE_NUM(I)
            IF(LINKAGE_OPERATOR(I) /= ' ') THEN
               CALL PROCESS_OPERATOR(LINKAGE_OPERATOR(I))
            ENDIF
            IF(GROUP_NUM(I) /= LAST_GROUP_NUM) THEN
               FIRST_GROUP_OCCURANCE = .TRUE.
               LAST_GROUP_NUM = GROUP_NUM(I)
            ENDIF
            IF(GROUP_NUM(I) > 0 .AND. FIRST_GROUP_OCCURANCE) THEN
               CALL SET_STACK_OPERATION_TO_VECTOR
               CALL SET_FUNC_VALUE(I)
               CALL PROCESS_OPERATOR("(")
               CALL PROCESS_OPERATOR("F")
               FIRST_GROUP_OCCURANCE = .FALSE.
               DO LEFT_BRACKET = 1, LEFT_BRACKET_COUNT(I)-1
                  CALL PROCESS_OPERATOR("(")
               ENDDO
            ELSE
               DO LEFT_BRACKET = 1, LEFT_BRACKET_COUNT(I)
                  CALL PROCESS_OPERATOR("(")
               ENDDO
            ENDIF
            CALL PROCESS_CONSTANT(MULTIPLIER(I))
            CALL PROCESS_OPERATOR("*")
            CALL PROCESS_VARIABLE(FLOAT(I))
            DO RIGHT_BRACKET = 1, RIGHT_BRACKET_COUNT(I)
               CALL PROCESS_OPERATOR(")")
            ENDDO
         ENDDO
         CALL CLOSE_POSTFIX_STACK
      ENDIF
      DEALLOCATE(PARTIAL_OBJFUNC_DES,GROUP_STRING,
     +         VARIABLE_STRING,
     +         LINKAGE_OPERATOR,DISCOUNT_START_PERIOD,
     +         VARIABLE_NUM,FUNCTION_OPERATOR,
     +         START_YEAR,END_YEAR,
     +         LEFT_BRACKET_COUNT,
     +         RIGHT_BRACKET_COUNT,
     +         GROUP_NUM,
     +         MULTIPLIER,DISCOUNT_RATE)
      RETURN
 1000 FORMAT(A)
      END
C
C
C
      SUBROUTINE INIT_POSTFIX_FORM
C
       use SpinDriftLib
      use prod_arrays_dimensions
      USE PROD_ARRAYS_DIMENSIONS
      INTEGER*2 OP_STACK_POINTR/0/,
     +          QUE_STACK_POINTR/0/
      CHARACTER*1 OPERATOR,S_OPERATOR
      LOGICAL*1 PRECEDENCE,STACK_OPERATOR_HAS_PRECEDENCE
      LOGICAL*1 VECTOR_OPERATION/.FALSE./
      INTEGER*2 R_FUNC_VALUE,FUNC_VALUE
      REAL VALUE
      INTEGER*2 OBJ_LINE_NUM/0/,R_OBJ_LINE_NUM
C
C DECLARATIONS FOR THE POSTFIX CALCULATION STACKS
C
      CHARACTER*1 OPERATOR_STACK(:)
      CHARACTER*1 QUE_TYPE(:)
      LOGICAL*1 QUE_VECTOR(:)
      INTEGER*2 QUE_POINTR(:)
      INTEGER*2 OPERATOR_OBJ_LINE_NUM(:)
      REAL QUE_INFO(:)
      ALLOCATABLE :: OPERATOR_STACK,
     +               QUE_TYPE,
     +               QUE_INFO,
     +               QUE_VECTOR,QUE_POINTR,
     +               OPERATOR_OBJ_LINE_NUM
      SAVE OPERATOR_STACK,QUE_TYPE,QUE_INFO,QUE_VECTOR,QUE_POINTR,
     +     OPERATOR_OBJ_LINE_NUM
C
         ALLOCATE(OPERATOR_STACK(0:256),
     +            OPERATOR_OBJ_LINE_NUM(0:256),
     +            QUE_TYPE(500),
     +            QUE_VECTOR(500),
     +            QUE_INFO(500),QUE_POINTR(500))
         QUE_INFO = 0.
      RETURN
C
      ENTRY SET_STACK_OPERATION_TO_VECTOR
         VECTOR_OPERATION = .TRUE.
      RETURN
C
      ENTRY SET_FUNC_VALUE(R_FUNC_VALUE)
         FUNC_VALUE = R_FUNC_VALUE
      RETURN   
C
      ENTRY SET_OBJ_LINE_NUM(R_OBJ_LINE_NUM)
         OBJ_LINE_NUM = R_OBJ_LINE_NUM
      RETURN
C
      ENTRY PROCESS_OPERATOR(OPERATOR)
         IF(OP_STACK_POINTR /= 0) THEN
            STACK_OPERATOR_HAS_PRECEDENCE = PRECEDENCE(OPERATOR,
     +                                  OPERATOR_STACK(OP_STACK_POINTR))
            DOWHILE (STACK_OPERATOR_HAS_PRECEDENCE)
               S_OPERATOR = OPERATOR_STACK(OP_STACK_POINTR)
               QUE_STACK_POINTR = QUE_STACK_POINTR + 1
               QUE_POINTR(QUE_STACK_POINTR) = 
     +                            OPERATOR_OBJ_LINE_NUM(OP_STACK_POINTR)
               IF(S_OPERATOR == 'F') THEN
                  QUE_TYPE(QUE_STACK_POINTR) = 'F'
                  QUE_INFO(QUE_STACK_POINTR) = FUNC_VALUE
                  QUE_VECTOR(QUE_STACK_POINTR) = .TRUE.
                  VECTOR_OPERATION = .FALSE.
               ELSE
                  QUE_TYPE(QUE_STACK_POINTR) = 'O'
                  QUE_INFO(QUE_STACK_POINTR) = INDEX('+-/*^',S_OPERATOR)
                  QUE_VECTOR(QUE_STACK_POINTR) = VECTOR_OPERATION
               ENDIF
               OP_STACK_POINTR = OP_STACK_POINTR - 1
               IF(OP_STACK_POINTR == 0) EXIT
               STACK_OPERATOR_HAS_PRECEDENCE = PRECEDENCE(OPERATOR,
     +                               OPERATOR_STACK(OP_STACK_POINTR))
            ENDDO
         ENDIF
         IF(OPERATOR /= ')') THEN
            OP_STACK_POINTR = OP_STACK_POINTR + 1
            OPERATOR_STACK(OP_STACK_POINTR) = OPERATOR
            OPERATOR_OBJ_LINE_NUM(OP_STACK_POINTR) = OBJ_LINE_NUM
         ELSEIF(OP_STACK_POINTR == 0) THEN
C
C STACK EMPTY ERROR
C
         ELSE
            OP_STACK_POINTR = OP_STACK_POINTR - 1
         ENDIF
      RETURN
      ENTRY PROCESS_CONSTANT(VALUE)
         QUE_STACK_POINTR = QUE_STACK_POINTR + 1
         QUE_TYPE(QUE_STACK_POINTR) = 'C'
         QUE_INFO(QUE_STACK_POINTR) = VALUE 
         QUE_VECTOR(QUE_STACK_POINTR) = VECTOR_OPERATION
         QUE_POINTR(QUE_STACK_POINTR) = OBJ_LINE_NUM
      RETURN
      ENTRY PROCESS_VARIABLE(VALUE)
         QUE_STACK_POINTR = QUE_STACK_POINTR + 1
         QUE_TYPE(QUE_STACK_POINTR) = 'V'
         QUE_INFO(QUE_STACK_POINTR) = VALUE 
         QUE_VECTOR(QUE_STACK_POINTR) = VECTOR_OPERATION
         QUE_POINTR(QUE_STACK_POINTR) = OBJ_LINE_NUM
      RETURN
      ENTRY CLOSE_POSTFIX_STACK
         DOWHILE (OP_STACK_POINTR > 0)
            S_OPERATOR = OPERATOR_STACK(OP_STACK_POINTR)
            QUE_STACK_POINTR = QUE_STACK_POINTR + 1
            QUE_POINTR(QUE_STACK_POINTR) = 
     +                            OPERATOR_OBJ_LINE_NUM(OP_STACK_POINTR)
            IF(S_OPERATOR == 'F') THEN
               QUE_TYPE(QUE_STACK_POINTR) = 'F'
               QUE_INFO(QUE_STACK_POINTR) = FUNC_VALUE
               QUE_VECTOR(QUE_STACK_POINTR) = VECTOR_OPERATION
               VECTOR_OPERATION = .FALSE.
            ELSE
               QUE_TYPE(QUE_STACK_POINTR) = 'O'
               QUE_INFO(QUE_STACK_POINTR) = INDEX('+-/*^',S_OPERATOR)
               QUE_VECTOR(QUE_STACK_POINTR) = VECTOR_OPERATION
            ENDIF
            OP_STACK_POINTR = OP_STACK_POINTR - 1
         ENDDO
         CALL RECEIVE_POSTFIX_INFO(QUE_STACK_POINTR,
     +                             QUE_TYPE,QUE_INFO,
     +                             QUE_VECTOR,QUE_POINTR)
         DEALLOCATE(OPERATOR_STACK,QUE_TYPE,QUE_INFO,QUE_VECTOR,
     +              QUE_POINTR,OPERATOR_OBJ_LINE_NUM)
      RETURN
      END
C
C
      FUNCTION PRECEDENCE(OPERATOR,STACK_OPERATOR)
C
      LOGICAL*1 PRECEDENCE
      CHARACTER*1 OPERATOR,STACK_OPERATOR
      INTEGER*2 PRIORITY_OF_STACK_OPERATOR,PRIORITY_OF_OPERATOR
C
      IF(STACK_OPERATOR == '(' .OR. OPERATOR == '(') THEN
         PRECEDENCE = .FALSE.
      ELSEIF(OPERATOR == ')') THEN
         IF(STACK_OPERATOR == '(') THEN
            PRECEDENCE = .FALSE.
         ELSE
            PRECEDENCE = .TRUE.
         ENDIF
      ELSE
         PRIORITY_OF_STACK_OPERATOR = INDEX("-*",STACK_OPERATOR)
         IF(PRIORITY_OF_STACK_OPERATOR == 0) THEN
            PRIORITY_OF_STACK_OPERATOR = INDEX("+/^",STACK_OPERATOR)
         ENDIF
         PRIORITY_OF_OPERATOR = INDEX("-*",OPERATOR)
         IF(PRIORITY_OF_OPERATOR == 0) THEN
            PRIORITY_OF_OPERATOR = INDEX("+/^",OPERATOR)
         ENDIF
         PRECEDENCE = PRIORITY_OF_STACK_OPERATOR > =PRIORITY_OF_OPERATOR
      ENDIF
      RETURN
      END

