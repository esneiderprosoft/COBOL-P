       Identification Division.
       Class-ID. FILEIO4 Inherits FJBASE.
       Environment Division.
       Configuration Section.
       Repository.
           Class FJBASE.
       Object.
       Environment Division.
       Input-Output Section.
       File-Control.
           Select Infile Assign To "test.txt"
                         Organization is Line Sequential
                         File Status is Infile-Status.
       Data Division.
       File Section.
       FD Infile.
       01 Infile-Record Pic X(132).
       Working-Storage Section.
       01 Infile-Status Pic 99.
       01 File-Opened-Flag Pic 9 Value 0.
       Procedure Division.

       Method-ID. OPEN-FILE.
       Data Division.
       Linkage Section.
       01 File-Operation Pic X(5).
       01 Return-Status Pic 99.
       01 Return-Record Pic X(132).
       Procedure Division Using File-Operation
                                Return-Status
                                Return-Record.
           If File-Opened-Flag Not = 0
               *> File is already open, so return error
               Move 99 To Return-Status
           Else
               Open Input Infile
               Move Infile-Status To Return-Status
               If Infile-Status = 0
                   Move 1 To File-Opened-Flag
               End-If
           End-If.
       End Method OPEN-FILE.
      
       Method-ID. CLOSE-FILE.
       Data Division.
       Linkage Section.
       01 File-Operation Pic X(5).
       01 Return-Status Pic 99.
       01 Return-Record Pic X(132).
       Procedure Division Using File-Operation
                                Return-Status
                                Return-Record.
           If File-Opened-Flag = 0
               *> File is not open, so return error
               Move 88 To Return-Status
           Else
               Close Infile
               Move Infile-Status To Return-Status
               Move 0 To File-Opened-Flag
           End-If.
       End Method CLOSE-FILE.
      
       Method-ID. READ-FILE.
       Data Division.
       Linkage Section.
       01 File-Operation Pic X(5).
       01 Return-Status Pic 99.
       01 Return-Record Pic X(132).
       Procedure Division using File-Operation
                                Return-Status
                                Return-Record.
           If File-Opened-Flag = 0
               *> File is not open, so return error
               Move 88 To Return-Status
           Else
               Read Infile
               Move Infile-Status to Return-Status
               Move Infile-Record to Return-Record
           End-If.
       End Method READ-FILE.
      
       End Object.
       End Class FILEIO4.
      
