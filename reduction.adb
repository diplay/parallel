with Ada.Float_Text_IO;		use Ada.Float_Text_IO;
with Ada.Text_IO;			use Ada.Text_IO;
with Ada.Integer_Text_IO;	use Ada.Integer_Text_IO;

with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;

procedure reduction is

	type vector is array(integer range <>) of float;
	
	length: constant integer := 1024;
	
	a0, d0, x0: vector(1..length);
	
	testa: vector(1..length);
	testx: vector(1..length);
	testd: vector(1..length);
	
	x00: float := 5.6;

	procedure fill is
	begin
		for i in 1..length loop
			a0(i):=float(i)/5000.0;
			d0(i):=float(2*i)/500.0;
			testa := a0;
			testd := d0;
		end loop;
	end fill;

	procedure test is
		t: integer := 0;
		eps : constant float := 0.01;
	begin
		testx:=(1=>testa(1)*x00+testd(1), others=>0.0);
		for i in 2..length loop
			testx(i):=testa(i)*testx(i-1) + testd(i);
		end loop;
		for i in 1..length loop
			if abs(x0(i)-testx(i)) > eps then
				put("Answers don't match:");
				put(i);
				put(x0(i), 8,5);
				put(testx(i),8,5);
				new_line;
				t := t + 1;
			end if;
		end loop;
		if t = 0 then
			put("Ok");
		end if;
	end test;

	procedure red(a: in out vector; d: in out vector; x: in out vector) is
		v1: vector(1..length);
		v2: vector(1..length);
		operation: integer;
		shift: integer;

		task type item is
			entry set(i: in integer);
			entry calculate;
			entry check;
		end item;

		unit: array(1..length) of item;

		task body item is
			id: integer;
		begin
			accept set(i: in integer) do
				id:=i;
			end set;
			v1(id) := 0.0;
			v2(id) := 0.0;
			loop
				select
					accept calculate;
					case operation is
					when 1 =>
						v2(id):=v1(id)+v2(id);
					when 2 =>
						v2(id):=v1(id)*v2(id);
					when 0 =>
						if id - shift>0 then
							v2(id):=v1(id-shift);
						else
							v2(id):=0.0;
						end if;
					when others =>
						null;
					end case;
				or
					accept check;
				or 
					terminate;
				end select;
			end loop;
		end item;

		function log2(n: in integer) return integer is
			ans, pow2: integer;
		begin
			pow2 := 1;
			ans := 0;
			while pow2 < n loop
				pow2 := pow2*2;
				ans := ans+1;
			end loop;
			return ans;
		end log2;

		procedure init is
		begin
			for i in 1..length loop
				unit(i).set(i);
			end loop;
		end init;
		
		procedure step is
		begin
			for i in 1..length loop
				unit(i).calculate;
			end loop;
			for i in 1..length loop
				unit(i).check;
			end loop;
		end step;

		function "+"(vv1: in vector; vv2: in vector) return vector is
		begin
			v1 := vv1;
			v2 := vv2;
			operation := 1;
			step;
			return v2;
		end "+";

		function "*"(vv1: in vector; vv2: in vector) return vector is
		begin
			v1 := vv1;
			v2 := vv2;
			operation := 2;
			step;
			return v2;
		end "*";

		function shiftr(v: in vector; l: in integer) return vector is
		begin
			v1 := v;
			shift := l;
			operation := 0;
			step;
			return v2;
		end shiftr;

	begin
		init;
		for l in 1..log2(length) loop
			x:= a*shiftr(x, 2**(l-1)) + x;
			a:= a*shiftr(a, 2**(l-1));
		end loop;
	end red;

begin
	fill;
	x0 := d0;
	x0(1) := a0(1)*x00 + d0(1);
	red(a0, d0, x0);
	test;
end reduction;

--Таракчян Левон К5-224
--Вывод программы:
--Ok
