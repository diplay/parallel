with Ada.Float_Text_IO;					use Ada.Float_Text_IO;
with Ada.Text_IO;						use Ada.Text_IO;

with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;

procedure iter_method is
	
	function f(x: in float) return float is
	begin
		return exp(x/2.0) - 1.0;
	end f;
	pragma INLINE(f);
	
	num_points : constant integer := 3;
	
	type points is array (1..num_points) of float;
	
	function iter(pt : in points; eps : in float) return float is
			
		task type calc_task is
			entry set(i: in integer);
			entry calculate;
			entry check;
		end calc_task;
		
		type tasks is array (1..num_points) of calc_task;
		
		processes: tasks;
		x: points := pt;
		new_x: points := (others => 0.0);
		
		task body calc_task is
			p1, p2:integer;
		begin
			accept set(i: in integer) do
				p1:=i;
			end set;
			
			p2 := (p1 mod num_points) + 1;

			loop
				select
					accept calculate;
					new_x(p1) := x(p1) - f(x(p1))*(x(p1) - x(p2))/(f(x(p1)) - f(x(p2)));
				or
					accept check;
				or
					terminate;
				end select;
			end loop;
		end calc_task;
		
		procedure init is
		begin
			for i in 1..num_points loop
				processes(i).set(i);
			end loop;
		end init;
		
		procedure step is
		begin
			for i in 1..num_points loop
				processes(i).calculate;
			end loop;
			for i in 1..num_points loop
				processes(i).check;
			end loop;
		end step;
		
		d : float;
		best : integer;
	
	begin
		init;
		loop
			step;
			d := Float'Last;
			for i in 1..num_points loop
				if abs(x(i) - new_x(i)) < d then
					d := abs(x(i) - new_x(i));
					best := i;
				end if;
			end loop;
			exit when d < eps;
			x := new_x;
		end loop;
		return (x(best) + new_x(best))/2.0;
	end iter;

	eps : constant float := 1.0E-5;

begin
	put("f(x) = exp(x/2.0) - 1.0");
	new_line;
	put("x = ");
	put(iter((-1.5, 1.0, 0.2), eps), 1, 3);
	put(" +- "); put(eps, 1, 1);
end iter_method;

--Таракчян Левон К5-224
--Вывод программы:
--f(x) = exp(x/2.0) - 1.0
--x = 5.162E-07 +- 1.0E-05
