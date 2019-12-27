       Identification Division.
       Program-ID. TESTFILEIO4.
       Environment Division.
       Configuration Section.
       Repository.
           Class FILEIO4.
       Data Division.
       Working-Storage Section.
       01 FILEIO-PTR Usage Object Reference.
       01 File-Operation Pic X(5).
       01 Return-Status Pic 99 Value 0.
       01 Return-Record Pic X(132).
       Procedure Division.
           Invoke FILEIO4 "NEW" Returning FILEIO-PTR.
           
           Invoke FILEIO-PTR "OPEN-FILE" Using File-Operation
                                               Return-Status
                                               Return-Record.
           If Return-Status = 0
               Perform Until Return-Status Not = 0
                   Invoke FILEIO-PTR "READ-FILE" Using File-Operation
                                                       Return-Status
                                                       Return-Record
                   If Return-Status = 0
                       Display Return-Record
                   End-If
               End-Perform
               Invoke FILEIO-PTR "CLOSE-FILE" Using File-Operation
                                                    Return-Status
                                                    Return-Record
           End-If.
           Set FILEIO-PTR To Null.
           Exit Program.
