
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;		-- Pour l'utilisation de fichier

package logger_pkg is

    type logger_t is protected

        procedure log_error(message : string := "");
        procedure log_note(message : string := "");
        procedure log_warning(message : string := "");
        procedure log_failure(message : string := "");
        procedure final_report;

        -- TODO : Complete
        procedure log_fichier_init(file_name : in string := "fichier_log.txt";
                                    write_file : in boolean := true);

        procedure write_fichier(message : in string := "new text");
        procedure set_verbosity(new_verbosity : severity_level := NOTE);

    end protected logger_t;

end logger_pkg;


package body logger_pkg is

    type logger_t is protected body

        variable nb_errors : integer := 0;
		variable nb_warning : integer := 0; -- ajout
        variable L: line;
        variable verbosity : severity_level := NOTE;
        variable enableLog : boolean := false;
        file fw : TEXT;

        procedure log_error(message: string := "") is
        begin
            --report message severity error;
            nb_errors := nb_errors + 1;
            if verbosity <= ERROR then
                report "Error nb" & integer'image(nb_errors) & ": " & message severity error;
                write_fichier("Error nb" & integer'image(nb_errors) & ": " & message);
            end if;
        end log_error;

        procedure log_warning(message: string := "") is
        begin
            --report message severity warning;
            nb_warning := nb_warning + 1;
            if verbosity <= WARNING then
                report "Warning nb" & integer'image(nb_warning) & ": " & message severity warning;
                --report message severity warning;
                write_fichier("Warning nb" & integer'image(nb_warning) & ": " & message);
            end if;
        end log_warning;

        procedure log_note(message: string := "") is
        begin
            if verbosity <= NOTE then
                report message severity note;
                write_fichier(message);
            end if;
        end log_note;

        procedure log_failure(message: string := "") is
        begin
            if verbosity <= FAILURE then
                report message severity failure;
                write_fichier(message);
            end if;
        end log_failure;

		procedure final_report is
        begin
            write(l,string'(""));
            writeline(fw,l);
			report "Nb errors = " & integer'image(nb_errors);
            report "Nb warning = " & integer'image(nb_warning);
            write_fichier("Nb errors = " & integer'image(nb_errors));
            write_fichier("Nb warning = " & integer'image(nb_warning));
            file_close(fw);
        end final_report;

        procedure log_fichier_init(file_name : in string := "fichier_log.txt";
                                    write_file : in boolean := true) is
        begin
            file_open(fw, file_name, write_mode);
            enableLog := write_file;
        end log_fichier_init;

        procedure write_fichier(message : in string := "new text") is
        begin
            if enableLog = true then
                WRITE(L,string'(message));
                WRITELINE(fw,L);
            end if;
                        --file fw: TEXT open WRITE_MODE is file_name;
        end write_fichier;

        procedure set_verbosity(new_verbosity : severity_level := NOTE) is
        begin
            verbosity := new_verbosity;
        end set_verbosity;

    end protected body logger_t;

end logger_pkg;
