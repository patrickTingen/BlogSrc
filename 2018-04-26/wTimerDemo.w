&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* TT for the generic timer OCX */
DEFINE TEMP-TABLE ttTimer NO-UNDO RCODE-INFORMATION
  FIELD cProc  AS CHARACTER
  FIELD iTime  AS INTEGER
  FIELD tNext  AS DATETIME
  INDEX idxNext IS PRIMARY tNext
  INDEX idxProc cProc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS SLIDER-1 FILL-IN-1 SLIDER-2 FILL-IN-2 ~
SLIDER-3 FILL-IN-3 
&Scoped-Define DISPLAYED-OBJECTS SLIDER-1 FILL-IN-1 SLIDER-2 FILL-IN-2 ~
SLIDER-3 FILL-IN-3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 13 BY 1
     FONT 0 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U INITIAL "/" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 4 BY 1
     FONT 0 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Nothing to see here" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 30 BY 1
     FONT 0 NO-UNDO.

DEFINE VARIABLE SLIDER-1 AS INTEGER INITIAL 0 
     VIEW-AS SLIDER MIN-VALUE 0 MAX-VALUE 2000 HORIZONTAL 
     TIC-MARKS BOTTOM FREQUENCY 250
     SIZE 46 BY 2.62 NO-UNDO.

DEFINE VARIABLE SLIDER-2 AS INTEGER INITIAL 0 
     VIEW-AS SLIDER MIN-VALUE 0 MAX-VALUE 500 HORIZONTAL 
     TIC-MARKS BOTTOM FREQUENCY 250
     SIZE 46 BY 2.62 NO-UNDO.

DEFINE VARIABLE SLIDER-3 AS INTEGER INITIAL 0 
     VIEW-AS SLIDER MIN-VALUE 0 MAX-VALUE 5 HORIZONTAL 
     TIC-MARKS BOTTOM FREQUENCY 1
     SIZE 46 BY 2.62 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     SLIDER-1 AT ROW 1.95 COL 10 NO-LABEL WIDGET-ID 2
     FILL-IN-1 AT ROW 2.91 COL 55 COLON-ALIGNED NO-LABEL WIDGET-ID 294
     SLIDER-2 AT ROW 5.29 COL 10 NO-LABEL WIDGET-ID 300
     FILL-IN-2 AT ROW 6.24 COL 55 COLON-ALIGNED NO-LABEL WIDGET-ID 298
     SLIDER-3 AT ROW 8.62 COL 10 NO-LABEL WIDGET-ID 304
     FILL-IN-3 AT ROW 9.57 COL 55 COLON-ALIGNED NO-LABEL WIDGET-ID 302
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 88.8 BY 11.62 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Timer demo"
         HEIGHT             = 11.62
         WIDTH              = 88.8
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 88.8
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 88.8
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FILL-IN-1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN-3:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 5.52
       COLUMN          = 72
       HEIGHT          = 1.43
       WIDTH           = 6
       WIDGET-ID       = 292
       HIDDEN          = yes
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(SLIDER-2:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Timer demo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Timer demo */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
/*------------------------------------------------------------------------------
    Name : pstimer.ocx.tick
    Desc : Execute timed procedure and schedule the next one
  ------------------------------------------------------------------------------*/

  /* Find the timer that caused the event */
  DEFINE BUFFER bTimer FOR ttTimer.

  /* Turn off events while handling events */
  chCtrlFrame:pstimer:ENABLED = FALSE.

  FIND FIRST bTimer NO-ERROR.
  IF AVAILABLE bTimer THEN
  DO:
    /* Run the proc */
    RUN VALUE(bTimer.cProc).

    /* When should it run again */
    IF AVAILABLE bTimer THEN
      bTimer.tNext = ADD-INTERVAL(NOW, bTimer.iTime,"milliseconds").
  END.

  /* Schedule the next event to run */
  RUN SetTimerInterval.

END PROCEDURE. /* OCX.psTimer.Tick */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SLIDER-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SLIDER-1 C-Win
ON VALUE-CHANGED OF SLIDER-1 IN FRAME DEFAULT-FRAME
DO:
  RUN setTimer('timedClock', SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SLIDER-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SLIDER-2 C-Win
ON VALUE-CHANGED OF SLIDER-2 IN FRAME DEFAULT-FRAME
DO:
  RUN setTimer('timedRotor', SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SLIDER-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SLIDER-3 C-Win
ON VALUE-CHANGED OF SLIDER-3 IN FRAME DEFAULT-FRAME
DO:
    fill-in-3:SCREEN-VALUE = 'Hello world'.
    RUN setTimer('timedTextHide', INTEGER(SELF:SCREEN-VALUE) * 1000).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "wTimerDemo.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "wTimerDemo.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  RUN control_load.
  DISPLAY SLIDER-1 FILL-IN-1 SLIDER-2 FILL-IN-2 SLIDER-3 FILL-IN-3 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE SLIDER-1 FILL-IN-1 SLIDER-2 FILL-IN-2 SLIDER-3 FILL-IN-3 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTimer C-Win 
PROCEDURE setTimer :
/*
 * Enable or disable a named timer.
 * Disable timer by setting piInterval to 0
 */
  DEFINE INPUT PARAMETER pcTimerProc AS CHARACTER NO-UNDO. /* name of timer */
  DEFINE INPUT PARAMETER piInterval  AS INTEGER   NO-UNDO. /* time in msec  */

  DEFINE BUFFER bTimer FOR ttTimer.

  /* Find it */
  FIND bTimer WHERE bTimer.cProc = pcTimerProc NO-ERROR.

  /* Create it if needed */
  IF NOT AVAILABLE bTimer THEN
  DO:
    CREATE bTimer.
    ASSIGN bTimer.cProc = pcTimerProc.
  END.

  /* When it is disabled, delete it */
  IF piInterval = 0 THEN
    DELETE bTimer.
  ELSE
  DO:
    ASSIGN
      bTimer.iTime = piInterval
      bTimer.tNext = ADD-INTERVAL(NOW, piInterval,"milliseconds")
      .
  END.

  /* Schedule the next event to run */
  RUN SetTimerInterval.

END PROCEDURE. /* setTimer */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTimerInterval C-Win 
PROCEDURE setTimerInterval :
/*
 * Set the interval of the timer so that it will tick exactly when the next timed event is due.
 */
  DEFINE BUFFER bTimer FOR ttTimer.

  /* Find the next timer to fire */
  FOR FIRST bTimer BY bTimer.tNext:

    /* How long until it should run? */
    chCtrlFrame:pstimer:INTERVAL = MAXIMUM(1,MTIME(bTimer.tNext) - MTIME(NOW)).

    /* Turn on events */
    chCtrlFrame:pstimer:ENABLED = TRUE.
  END.

END PROCEDURE. /* setTimerInterval */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE timedClock C-Win 
PROCEDURE timedClock :
/* Show the time
  */
  FILL-IN-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TIME,'hh:mm:ss').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE timedRotor C-Win 
PROCEDURE timedRotor :
/* Show rotating thingy
*/
  CASE FILL-IN-2:
      WHEN '/' THEN FILL-IN-2 = '-'.
      WHEN '-' THEN FILL-IN-2 = '\'.
      WHEN '\' THEN FILL-IN-2 = '|'.
      WHEN '|' THEN FILL-IN-2 = '/'.
  END CASE.
  DISPLAY FILL-IN-2 WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE timedTextHide C-Win 
PROCEDURE timedTextHide :
/* Hide text after x seconds
  */
  DO WITH FRAME {&FRAME-NAME}:
    fill-in-3:SCREEN-VALUE = 'Nothing to see here'.
    SLIDER-3:SCREEN-VALUE = '0'.
    RUN setTimer('timedTextHide',0).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

