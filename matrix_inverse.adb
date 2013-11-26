with Ada.Float_Text_IO;		use Ada.Float_Text_IO;
with Ada.Text_IO;			use Ada.Text_IO;
with Ada.Integer_Text_IO;	use Ada.Integer_Text_IO;

with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;

with Ada.Numerics.Discrete_Random;

procedure matrix_inverse is

	type matrix is array(integer range <>, integer range <>) of Long_Long_Float;
	
	proc_num: integer := 5;
	dim: constant integer := 10;
	a: matrix(1..dim, 1..dim);

	procedure fill is
		subtype Range1_10 is Positive range 1..10;
		package Rand is
			new Ada.Numerics.Discrete_Random(Range1_10);
		seed : Rand.Generator;
	begin
		Rand.Reset(seed);
		for i in 1..dim loop
			for j in 1..dim loop
				a(i,j) := Long_Long_Float(Rand.Random(seed))/100.0;
				if i>j then
					a(i,j) := 0.0;
				end if;
			end loop;
		end loop;
	end fill;

	procedure test(inverted: in matrix) is
		eps : constant Long_Long_Float := 0.5;
		fail_flag : boolean;
		c: matrix(1..dim, 1..dim);
	begin
		c := (others => (others => 0.0));
		for i in 1..dim loop
			for j in 1..dim loop
				for k in 1..dim loop
					c(i, j) := c(i, j) + a(i, k) * inverted(k,j);
				end loop;
			end loop;
		end loop;
		for i in 1..dim loop
			for j in 1..dim loop
				if(i = j) and abs(c(i, j) - 1.0) > eps then
					put("Error("); put(i);
					put(j);
					put(") =");
					put(float(c(i,j)),5,3);
					new_line;
					fail_flag := true;
				end if;
				if(i /= j) and abs(c(i, j)) > eps then
					put("Error("); put(i);
					put(j);
					put(") =");
					put(float(c(i,j)),5,3);
					new_line;
					fail_flag := true;
				end if;
			end loop;
		end loop;
		if fail_flag then
			put("Test Failed");
		else
			put("Ok");
		end if;
	end test;

	function inv(a: in matrix; proc_num: in integer) return matrix is
	
		h:integer;
		inverted: matrix(1..dim, 1..dim);
		
		task type part is
			entry set(left, right:in integer);
		end part;
		
		parts: array(1..proc_num) of part;
		
		task body part is
			l, r: integer;
			s: Long_Long_Float;
		begin
			accept set(left,right: in integer) do
				l := left;
				r := right;
			end set;
			for col in l..r loop
				for row in reverse 1 .. col - 1 loop
					s := 0.0;
					for j in row + 1 .. col loop
						s := s + a(row, j)*inverted(j, col);
					end loop;
					inverted(row, col) := - s*inverted(row, row);
				end loop;
			end loop;
		end part;

	begin
		inverted := (others => (others => 0.0));
		h := dim/proc_num;
		for i in 1..dim loop
			inverted(i, i) := 1.0/a(i, i);
		end loop;
		for i in 1..proc_num loop
			parts(i).set((i - 1)*h + 1, i*h);
		end loop;
		return(inverted);
	end inv;

begin
	fill;
	test(inv(a, proc_num));
end matrix_inverse;

--Таракчян Левон К5-224
--Вывод программы:
--Ok
--
--Или, например:
--Error(          1         10) =    5.556E-01
--Error(          2          9) =   -1.331E+00
--Error(          2         10) =    6.667E-01
--Error(          3          9) =   -7.762E-01
--Error(          6         10) =    7.778E-01
--Error(          8         10) =    8.889E-01
--Test Failed
