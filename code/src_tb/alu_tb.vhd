--------------------------------------------------------------------------------
-- HEIG-VD
-- Haute Ecole d'Ingenerie et de Gestion du Canton de Vaud
-- School of Business and Engineering in Canton de Vaud
--------------------------------------------------------------------------------
-- REDS Institute
-- Reconfigurable Embedded Digital Systems
--------------------------------------------------------------------------------
--
-- File     : alu_tb.vhd
-- Author   : TbGenerator
-- Date     : 08.03.2018
--
-- Context  :
--
--------------------------------------------------------------------------------
-- Description : This module is a simple VHDL testbench.
--               It instanciates the DUV and proposes a TESTCASE generic to
--               select which test to start.
--
--------------------------------------------------------------------------------
-- Dependencies : -
--
--------------------------------------------------------------------------------
-- Modifications :
-- Ver   Date        Person     Comments
-- 0.1   08.03.2018  TbGen      Initial version
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all; -- Pour l'aleatoire

library project_lib;
context project_lib.project_ctx;


entity alu_tb is
    generic (
        TESTCASE : integer := 0;
        SIZE     : integer := 8;
        ERRNO    : integer := 0
    );

end alu_tb;

architecture testbench of alu_tb is

    signal a_sti    : std_logic_vector(SIZE-1 downto 0);
    signal b_sti    : std_logic_vector(SIZE-1 downto 0);
    signal s_obs    : std_logic_vector(SIZE-1 downto 0);
    signal c_obs    : std_logic;
    signal mode_sti : std_logic_vector(2 downto 0);

    signal sim_end_s : boolean := false;

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

    function Aleatoire_SIZE(v_seed1: in positive; v_seed2: in positive) return std_logic_vector is
        -- 2 seeds pour la génération aléatoire
        variable seed1: positive;
        variable seed2: positive;
        -- valeur aléatoire entre 0 et 1.0
        variable rand: real;
        -- valeur aléatoire entre 0 et 65535
        variable int_rand: integer;
        -- stimulus alétatoire sur 16 bits
        variable stim: std_logic_vector(SIZE-1 downto 0);

        begin
            seed1 := v_seed1;
            seed2 := v_seed2;
            UNIFORM(seed1, seed2, rand);
            -- troncature du nombre après changement d'échelle
            int_rand := INTEGER(TRUNC(rand*real(2**SIZE)));
            --int_rand := INTEGER(TRUNC(rand*256.0));
            -- conversion vers std_logic_vector
            stim := std_logic_vector(to_unsigned(int_rand, stim'LENGTH));
            --stim := std_logic_vector(to_unsigned(23, stim'LENGTH));
            return stim;
    end Aleatoire_SIZE;

begin

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

        -- Add input generator
        procedure gen_input(a : in std_logic_vector(SIZE-1 downto 0);
                            b : in std_logic_vector(SIZE-1 downto 0);
                            mode : in std_logic_vector(2 downto 0)) is
        begin
            a_sti    <= a;
            b_sti    <= b;
            mode_sti <= mode;

        end  procedure gen_input;

        procedure verif_alu is
        begin
            case( mode_sti ) is

                when "000" =>
                    if (std_logic_vector(s_obs) = std_logic_vector(unsigned(a_sti)+unsigned(b_sti))) then
                        logger.log_error("correct");
                    else
                        logger.log_error("faux");
                    end if;

                when others =>
                    logger.log_error("autre");
            end case;

        end  procedure verif_alu;

    begin

        -- a_sti    <= default_value;
        -- b_sti    <= default_value;
        -- mode_sti <= default_value;

        --gen_input("10100101","10101010","000");
        --wait for 2ns;
        --verif_alu;

        --wait for 50ns;

        -- Generation de toutes les possibilites
        for I in 0 to (2**mode_sti'LENGTH)-1 loop
            for J in 0 to (2**b_sti'LENGTH)-1 loop
                for K in 0 to (2**a_sti'LENGTH)-1 loop
                    gen_input(Std_logic_Vector(To_Unsigned(K,a_sti'LENGTH)),Std_logic_Vector(To_Unsigned(J,b_sti'LENGTH)), Std_logic_Vector(To_Unsigned(I,mode_sti'LENGTH)));
                    wait for 50ns;
                end loop;
            end loop;
        end loop;

        -- Aleatoire 1000 fois
        for L in 0 to 10 loop
            test := L;
            gen_input(Aleatoire_SIZE(test, 234),Aleatoire_SIZE(12, test), "000");
            wait for 50ns;
        end loop;

        -- do something
        case TESTCASE is
            when 0      => -- default testcase
            when others => report "Unsupported testcase : "
                                  & integer'image(TESTCASE)
                                  severity error;
        end case;

        -- end of simulation
        sim_end_s <= true;

        -- stop the process
        wait;

    end process; -- stimulus_proc

end testbench;
