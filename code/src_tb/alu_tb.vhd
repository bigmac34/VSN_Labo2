-------------------------------------------------------------------------------
-- HES-SO Master
-------------------------------------------------------------------------------
-- Cours VSN
--------------------------------------------------------------------------------
--
-- File     : alu_tb.vhd
-- Author   : Jérémie Macchi
-- Date     : 17.03.2018
--
-- Context  :
--
--------------------------------------------------------------------------------
-- Description : testbench pour la verification d'une alu
--
--------------------------------------------------------------------------------
-- Dependencies : -
--
--------------------------------------------------------------------------------
-- Modifications :
-- Ver   Date        	Person     		Comments
-- 0.1   08.03.2018  	TbGen      		Initial version
-- 1.0	 08.03.2012		Jérémie Macchi	Labo 2 de VSN
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all; -- Lib. pour l'aleatoire

library project_lib;
context project_lib.project_ctx;


entity alu_tb is
    generic (
        TESTCASE 	: integer := 0;
	    SIZE     	: integer := 8;
        ERRNO		: integer := 0;
        C_SEED1		: positive := 3145;
        C_SEED2		: positive := 1123
    );

end alu_tb;

architecture testbench of alu_tb is

	-------------
	-- Siganux --
	-------------
    signal a_sti    : std_logic_vector(SIZE-1 downto 0);
    signal b_sti    : std_logic_vector(SIZE-1 downto 0);
    signal s_obs    : std_logic_vector(SIZE-1 downto 0);
    signal c_obs    : std_logic;
    signal mode_sti : std_logic_vector(2 downto 0);
    signal sim_end_s : boolean := false;
	signal sim_synchro_s : boolean := false;

	-------------------
	-- Composant alu --
	-------------------
    component alu is
    generic (
        SIZE  : integer := 8;
        ERRNO : integer := 0
    );
    port (
        a_i    : in std_logic_vector(SIZE-1 downto 0);
        b_i    : in std_logic_vector(SIZE-1 downto 0);
        s_o    : out std_logic_vector(SIZE-1 downto 0);
        c_o    : out std_logic;
        mode_i : in std_logic_vector(2 downto 0)
    );
    end component;

begin

	------------------------
	-- Mapping avec l'alu --
	------------------------
    duv : alu
    generic map (
        SIZE  => SIZE,
        ERRNO => ERRNO
    )
    port map (
        a_i    => a_sti,
        b_i    => b_sti,
        s_o    => s_obs,
        c_o    => c_obs,
        mode_i => mode_sti
    );

    stimulus_proc: process is
		--------------------------------
        -- Variables pour l'aleatoire --
		--------------------------------
        variable seed1: positive := C_SEED1;
        variable seed2: positive := C_SEED2;
        variable rand: real;
        variable int_rand: integer;
        variable stim: std_logic_vector(SIZE-1 downto 0);
        variable stim1: std_logic_vector(SIZE-1 downto 0);
        variable stim2: std_logic_vector(SIZE-1 downto 0);

		----------------------------------------------
        -- Procedure pour la generation des signaux --
		----------------------------------------------
        procedure gen_input(a : in std_logic_vector(SIZE-1 downto 0);
                            b : in std_logic_vector(SIZE-1 downto 0);
                            mode : in std_logic_vector(2 downto 0)) is
        begin
            a_sti    <= a;
            b_sti    <= b;
            mode_sti <= mode;

        end  procedure gen_input;

		----------------------------------------------
		-- Procedure pour la gestion de l'aleatoire --
		----------------------------------------------
        procedure aleatoire_SIZE is
            begin
                UNIFORM(seed1, seed2, rand);
                -- troncature du nombre après changement d'échelle
                int_rand := INTEGER(TRUNC(rand*real(2**SIZE)));
                -- conversion vers std_logic_vector
                stim := std_logic_vector(to_unsigned(int_rand, stim'LENGTH));
        end aleatoire_SIZE;

	-------------------------------------
	-- Partie principale du test bench --
	-------------------------------------
    begin
		-- TESTCASE choisi lors de la compilation
        case TESTCASE is
			-- Generation de toutes les possibilites
            when 0  =>
                for I in 0 to (2**mode_sti'LENGTH)-1 loop
                    for J in 0 to (2**b_sti'LENGTH)-1 loop
                        for K in 0 to (2**a_sti'LENGTH)-1 loop
                            gen_input(Std_logic_Vector(To_Unsigned(K,a_sti'LENGTH)),Std_logic_Vector(To_Unsigned(J,b_sti'LENGTH)), Std_logic_Vector(To_Unsigned(I,mode_sti'LENGTH)));
                            wait for 25 ns;
							sim_synchro_s <= true,
											false after 1 ns;
                            wait for 24 ns;
                        end loop;
                    end loop;
                end loop;

			-- Aleatoire 100 fois pour chaque mode
            when 1	=>
				for I in 0 to (2**mode_sti'LENGTH)-1 loop
					for J in 0 to 100 loop
						aleatoire_SIZE;
						stim1 := stim;
						aleatoire_SIZE;
						stim2 := stim;
						gen_input(stim1, stim2, Std_logic_Vector(To_Unsigned(I,mode_sti'LENGTH)));
						wait for 25 ns;
						sim_synchro_s <= true,
										false after 1 ns;
						wait for 24 ns;
					end loop;
				end loop;

			-- Combinaision de toutes les possibilites et de l'aleatoire
			when 2	=>
				-- Generation de toutes les possibilites
				for I in 0 to (2**mode_sti'LENGTH)-1 loop
					for J in 0 to (2**b_sti'LENGTH)-1 loop
						for K in 0 to (2**a_sti'LENGTH)-1 loop
							gen_input(Std_logic_Vector(To_Unsigned(K,a_sti'LENGTH)),Std_logic_Vector(To_Unsigned(J,b_sti'LENGTH)), Std_logic_Vector(To_Unsigned(I,mode_sti'LENGTH)));
							wait for 25 ns;
							sim_synchro_s <= true,
											false after 1 ns;
							wait for 24 ns;
						end loop;
					end loop;
				end loop;

				-- Aleatoire 100 fois pour chaque mode
				for I in 0 to (2**mode_sti'LENGTH)-1 loop
					for J in 0 to 100 loop
						aleatoire_SIZE;
						stim1 := stim;
						aleatoire_SIZE;
						stim2 := stim;
						gen_input(stim1, stim2, Std_logic_Vector(To_Unsigned(I,mode_sti'LENGTH)));
						wait for 25 ns;
						sim_synchro_s <= true,
										false after 1 ns;
						wait for 24 ns;
					end loop;
				end loop;

            when others => report "Unsupported testcase : "
                                  & integer'image(TESTCASE)
                                  severity error;
        end case;

        -- end of simulation
        sim_end_s <= true;

        -- stop the process
        wait;

    end process; -- stimulus_proc


	--------------------------------
	-- Processus de vérification --
	-------------------------------
	process

	-- Variables pour la verification de l'alu
	variable a_v: std_logic_vector(SIZE downto 0);
	variable b_v: std_logic_vector(SIZE downto 0);
	variable s_v: std_logic_vector(SIZE downto 0);
	variable c_v: std_logic;
	variable operateur: string (1 to 5);

	--------------------------------
	-- Processus de vérification --
	-------------------------------
	begin

		while not sim_end_s loop
			wait until rising_edge(sim_synchro_s) or sim_end_s;
			-- Initialisation
			a_v := '0' & a_sti;
			b_v := '0' & b_sti;

			------------------------------------------------
			-- Calcul des resultats en fonction des modes --
			------------------------------------------------
			case( mode_sti ) is
				when "000" =>
				s_v := std_logic_vector(unsigned(a_v)+unsigned(b_v));
				c_v := s_v(SIZE);
				operateur := "  +  ";

				when "001" =>
				s_v := std_logic_vector(unsigned(a_v)-unsigned(b_v));
				c_v := s_v(SIZE);
				operateur := "  -  ";

				when "010" =>
				s_v := a_v or b_v;
				operateur := "  or ";

				when "011" =>
				s_v := a_v and b_v;
				operateur := " and ";

				when "100" =>
				s_v := a_v;
				operateur := "     ";

				when "101" =>
				s_v := b_v;
				operateur := "     ";

				when "110" =>
				if (a_v = b_v) then
					s_v(0) := '1';
				else
					s_v(0) := '0';
				end if;
				operateur := " /=  ";

				when "111" =>
				s_v := (others => '0');

				when others =>
					logger.log_error("autre");
			end case;

			-- Detection des erreurs pour tout les modes sauf "110"
			if (s_v(SIZE-1 downto 0) /= std_logic_vector(s_obs)) then
				if(unsigned(mode_sti) < 2) then
					logger.log_error(integer'image(to_integer(unsigned(a_sti))) & operateur(2 to 4) & integer'image(to_integer(unsigned(b_sti))) & " /= " &  integer'image(to_integer(unsigned(s_obs))));
				elsif(unsigned(mode_sti) < 4) then
					logger.log_error(integer'image(to_integer(unsigned(a_sti))) & operateur & integer'image(to_integer(unsigned(b_sti))) & " /= " &  integer'image(to_integer(unsigned(s_obs))));
				elsif((unsigned(mode_sti) < 6) or (unsigned(mode_sti) = 7)) then
					logger.log_error(integer'image(to_integer(unsigned(s_obs))) & " /= " &  integer'image(to_integer(unsigned(s_v))));
				end if;
			end if;

			-- Detection des erreurs pour le mode "110"
			if(unsigned(mode_sti) = 6) then
				if (s_obs(0)/= s_v(0)) then
					logger.log_error(std_logic'image(s_obs(0)) & " /= " &  std_logic'image(s_v(0)));
				end if;
			end if;

			-- Detection des erreurs de retenue
			if (c_v /= c_obs) then
				-- Que pour les modes "000" et "001"
				if(unsigned(mode_sti) < 2) then
					logger.log_error("Retenue:" & std_logic'image((c_obs)) & " /= " & std_logic'image(((c_v))));
				end if;
			end if;
		end loop;
		-- stop the process
		wait;
	end process;

end testbench;
