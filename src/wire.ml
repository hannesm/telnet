
[%%cenum
type telnet_command =
  | SUBNEG_END [@id 240] (* SE *)
  | NOP [@id 241]
  | DATA_MARK [@id 242] (* The data stream portion of a Synch. This should always be accompanied by a TCP Urgent notification. *)
  | BREAK [@id 243] (* BRK *)
  | INTERRUPT_PROCESS [@id 244]
  | ABORT_OUTPUT [@id 245]
  | ARE_YOU_THERE [@id 246]
  | ERASE_CHARACTER [@id 247]
  | ERASE_LINE [@id 248]
  | GO_AHEAD [@id 249]
  | SUBNEG [@id 250]
  | WILL [@id 251] (* option code *)
  | WILL_NOT [@id 252] (* option code *)
  | DO [@id 253] (* option code *)
  | DO_NOT [@id 254]
  | IAC [@id 255] (* Data Byte 255. *)
  [@@uint8_t] [@@sexp]
]

[%%cenum
type telnet_option =
  | Binary_Transmission
  | Echo
  | Reconnection
  | Suppress_Go_Ahead
  | Approx_Message_Size_Negotiation
  | Status
  | Timing_Mark
  | Remote_Controlled_Trans_and_Echo
  | Output_Line_Width
  | Output_Page_Size
  | Output_Carriage_Return_Disposition
  | Output_Horizontal_Tab_Stops
  | Output_Horizontal_Tab_Disposition
  | Output_Formfeed_Disposition
  | Output_Vertical_Tabstops
  | Output_Vertical_Tab_Disposition
  | Output_Linefeed_Disposition
  | Extended_ASCII
  | Logout
  | Byte_Macro
  | Data_Entry_Terminal
  | SUPDUP
  | SUPDUP_Output
  | Send_Location
  | Terminal_Type
  | End_of_Record
  | TACACS_User_Identification
  | Output_Marking
  | Terminal_Location_Number
  | Telnet_3270_Regime
  | X_3_PAD
  | Negotiate_About_Window_Size
  | Terminal_Speed
  | Remote_Flow_Control
  | Linemode
  | X_Display_Location
  | Environment_Option
  | Authentication_Option
  | Encryption_Option
  | New_Environment_Option
  | TN3270E
  | XAUTH
  | CHARSET
  | Telnet_Remote_Serial_Port
  | Com_Port_Control_Option
  | Telnet_Suppress_Local_Echo
  | Telnet_Start_TLS
  | KERMIT
  | SEND_URL
  | FORWARD_X
  (* 50-137,Unassigned *)
  | TELOPT_PRAGMA_LOGON [@id 138]
  | TELOPT_SSPI_LOGON
  | TELOPT_PRAGMA_HEARTBEAT
  (* 141-254 *)
  | Extended_Options_List [@id 255]
  [@@uint8_t] [@@sexp]
]
