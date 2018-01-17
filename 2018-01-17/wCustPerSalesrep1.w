&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports           PROGRESS
*/
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

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brCustomer

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Customer Salesrep

/* Definitions for BROWSE brCustomer                                    */
&Scoped-define FIELDS-IN-QUERY-brCustomer Customer.Cust-Num Customer.Name 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brCustomer 
&Scoped-define QUERY-STRING-brCustomer FOR EACH Customer ~
      WHERE Customer.Sales-Rep = salesrep.Sales-Rep NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brCustomer OPEN QUERY brCustomer FOR EACH Customer ~
      WHERE Customer.Sales-Rep = salesrep.Sales-Rep NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brCustomer Customer
&Scoped-define FIRST-TABLE-IN-QUERY-brCustomer Customer


/* Definitions for BROWSE brSalesrep                                    */
&Scoped-define FIELDS-IN-QUERY-brSalesrep Salesrep.Sales-Rep ~
Salesrep.Rep-Name 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brSalesrep 
&Scoped-define QUERY-STRING-brSalesrep FOR EACH Salesrep NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brSalesrep OPEN QUERY brSalesrep FOR EACH Salesrep NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brSalesrep Salesrep
&Scoped-define FIRST-TABLE-IN-QUERY-brSalesrep Salesrep


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brCustomer}~
    ~{&OPEN-QUERY-brSalesrep}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brSalesrep brCustomer 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brCustomer FOR 
      Customer SCROLLING.

DEFINE QUERY brSalesrep FOR 
      Salesrep SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brCustomer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brCustomer C-Win _STRUCTURED
  QUERY brCustomer NO-LOCK DISPLAY
      Customer.Cust-Num FORMAT ">>>>9":U
      Customer.Name FORMAT "x(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 48 BY 8.33 FIT-LAST-COLUMN.

DEFINE BROWSE brSalesrep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brSalesrep C-Win _STRUCTURED
  QUERY brSalesrep NO-LOCK DISPLAY
      Salesrep.Sales-Rep FORMAT "x(4)":U
      Salesrep.Rep-Name FORMAT "x(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 38 BY 8.33 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brSalesrep AT ROW 1 COL 1 WIDGET-ID 300
     brCustomer AT ROW 1 COL 39 WIDGET-ID 200
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 138.2 BY 8.76 WIDGET-ID 100.


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
         TITLE              = "Customer per sales rep"
         HEIGHT             = 8.29
         WIDTH              = 85.8
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 138.2
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 138.2
         RESIZE             = no
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
/* BROWSE-TAB brSalesrep 1 DEFAULT-FRAME */
/* BROWSE-TAB brCustomer brSalesrep DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brCustomer
/* Query rebuild information for BROWSE brCustomer
     _TblList          = "sports.Customer"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "Customer.Sales-Rep = salesrep.Sales-Rep"
     _FldNameList[1]   = sports.Customer.Cust-Num
     _FldNameList[2]   = sports.Customer.Name
     _Query            is OPENED
*/  /* BROWSE brCustomer */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brSalesrep
/* Query rebuild information for BROWSE brSalesrep
     _TblList          = "sports.Salesrep"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = sports.Salesrep.Sales-Rep
     _FldNameList[2]   = sports.Salesrep.Rep-Name
     _Query            is OPENED
*/  /* BROWSE brSalesrep */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Customer per sales rep */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Customer per sales rep */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brSalesrep
&Scoped-define SELF-NAME brSalesrep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brSalesrep C-Win
ON VALUE-CHANGED OF brSalesrep IN FRAME DEFAULT-FRAME
DO:
  {&OPEN-QUERY-brCustomer}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brCustomer
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
  APPLY 'value-changed' TO brSalesrep.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  ENABLE brSalesrep brCustomer 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

