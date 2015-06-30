define ack (); 
define ack (M, N)
{
   !if (M)
     return( N + 1 );
   !if (N)
     return ( ack(M-1, 1) );
   return( ack(M-1, ack(M, N-1)) );
}


define main()
{
   variable NUM = 1;
   if (__argc > 1) NUM = integer (__argv[1]);
   fprintf (stdout, "Ack(3,%d): %d\n", (NUM, ack(3, NUM)));
}

main()
define main ()
{
   variable n = 1;
   if (__argc > 1) n = integer (__argv[1]);
   variable x = [1:n];
   variable y = Int_Type[n];

   loop (1000)
     y += x;

   fprintf(stdout, "%u %u\n", y[0], y[-1], 2);
}
main ();
#!/usr/bin/env slsh
% The Computer Language Shootout Benchmarks
% http://shootout.alioth.debian.org/
%
% contributed by John E. Davis

typedef struct { left, right, item } TreeNode_Type;

private define item_check ();
private define item_check (tree)
{
   variable left = tree.left;
   if (left == NULL)
     return tree.item;

   return tree.item + item_check(left) - item_check (tree.right);
}

private define bottomup_tree ();
private define bottomup_tree (item, depth)
{   
   variable s = @TreeNode_Type;
   !if (depth)
     {
	s.item = item;
	return s;
     }
   depth--;
   set_struct_fields (s, bottomup_tree (2 * item - 1, depth),
		      bottomup_tree (2 * item, depth), item);
   return s;
}

define slsh_main ()
{
   variable N = 1;
   if (__argc > 1) N = integer (__argv[1]);

   variable max_depth = N, min_depth = 4;

   if (min_depth + 2 > N)
     max_depth = min_depth + 2;

   variable stretch_depth = max_depth + 1;
   variable stretch_tree = bottomup_tree(0, stretch_depth);
   () = fprintf (stdout, "stretch tree of depth %u\t check: %ld\n",
		 stretch_depth, item_check (stretch_tree));
   stretch_tree = NULL;

   variable longlived_tree = bottomup_tree (0, max_depth);

   _for (min_depth, max_depth, 2)
    {
       variable depth = ();
       variable iterations = 1 shl (max_depth - depth + min_depth);
       variable check = 0;

       _for (1, iterations, 1)
	 {
	    variable i = ();
            variable tmp = bottomup_tree (i, depth);
            check += item_check (tmp);
	    tmp = NULL;
            tmp = bottomup_tree (-i, depth);
            check += item_check (tmp);
	    tmp = NULL;
        }

       () = fprintf (stdout, "%d\t trees of depth %u\t check: %d\n",
		     iterations * 2, depth, check);
    }

   () = fprintf (stdout, "long lived tree of depth %u\t check: %d\n",
		 max_depth, item_check (longlived_tree));
}
% Stricly speaking, slang does not have exceptions.  It uses error blocks.
variable HI = 0, LO = 0;
variable  Hi_exception = 1;
variable  Lo_exception = 2;
variable Exception;

()=fclose (stderr);
_traceback=0;
define throw (e)
{
   Exception = e;
   error ("");
}
 
define blowup (n) 
{
   if (n & 1) 
     throw (Lo_exception);

   throw (Hi_exception);
}

define lo_function (n)
{
   ERROR_BLOCK
     {
	if (Exception == Lo_exception)
	  {
	     LO++;
	     _clear_error ();
	  }
     }
   blowup(n);
}

define hi_function (n)
{
   ERROR_BLOCK
     {
	if (Exception == Hi_exception)
	  {
	     HI++;
	     _clear_error ();
	  }
     }
   lo_function(n);
}

define some_function (n)
{
   hi_function(n);
}

define main ()
{
   variable N = 1;
   if (__argc == 2) N = integer (__argv[1]);

   while (N) 
     {
	some_function(N);
	N--;
     }
    fprintf (stdout, "Exceptions: HI=%d / LO=%d\n", HI, LO);
}
main ();
#!/usr/bin/env slsh
% The Computer Language Shootout Benchmarks
% http://shootout.alioth.debian.org/
%
% contributed by John E. Davis
private define fannkuch (n)
{
   variable m = n-1;
   variable perm1 = [0:m];
   variable r0, r1, r = n;
   variable count = Int_Type[n];
   variable max_flipcounts = 0;
   variable check = 30;
   forever
     {
	if (check)
	  {
	     message(strjoin (array_map(String_Type, &string, perm1+1),""));
	     check--;
	  }
	loop (r-1) { r; r--; count[r] = (); }

	!if ((perm1[0] == 0) or (perm1[m] == m))
	  {
	     variable perm = @perm1;
	     variable flipcounts = 0;
	     forever 
	       {
		  variable k = perm[0];
		  !if (k)
		    break;
		  array_reverse (perm, 0, k);
		  flipcounts++;
	       }
	     if (flipcounts > max_flipcounts)
	       max_flipcounts = flipcounts;
	  }	
	forever
	  {
	     if (r == n)
	       return max_flipcounts;

	     variable perm0 = perm1[0];
	     r0 = 0, r1 = 1; 
	     loop (r) { perm1[r0] = perm1[r1]; r0++; r1++; }
	     perm1[r] = perm0;
	     variable cr = count[r]-1;
	     count[r] = cr;
	     if (cr > 0)
	       break;
	     r++;
	  }
     }
}

define slsh_main ()
{
   variable num = 1;
   if (__argc > 1) num = integer (__argv[1]);
   vmessage ("Pfannkuchen(%d) = %ld", num, fannkuch (num));
}
   
#!/usr/bin/env slsh
% The Computer Language Shootout Benchmarks
% http://shootout.alioth.debian.org/
%
% contributed by John E. Davis

private variable IM = 139968;
private variable IA = 3877;
private variable IC = 29573;
private variable Last = 42;

private define random (max)
{
   Last = (Last * IA + IC) mod IM;
   (max * Last) / IM;
}

private define select_random (table, n)
{
   variable p = table.prob;
   variable np = length (p);
   variable ch = table.ch;
   variable a = Char_Type[n];
   variable p0 = p[0], ch0 = ch[0];
   _for (0, n-1, 1)
     {
	variable i = ();
	variable r = random (1.0);
	if (r < p0)
	  {
	     a[i] = ch0;
	     continue;
	  }
	a[i] = ch[wherefirst(r < p)];
     }
   return a;
}

private define make_random_fasta (id, desc, table, n)
{
   () = fprintf (stdout, ">%s %s\n", id, desc);
   variable width = 60;
   variable todo = n;
   while (todo >= width)
     {
	() = fwrite (select_random (table, width), stdout);
	() = fputs ("\n", stdout);
	todo -= width;
     }
   if (todo)
     {
	() = fwrite (select_random (table, todo), stdout);
	() = fputs ("\n", stdout);
     }
}

private define make_repeat_fasta (id, desc, s, n)
{
   () = fprintf (stdout, ">%s %s\n", id, desc);
   variable width = 60;
   variable todo = n;
   variable len = strlen (s);

   variable i = 0;
   while (todo)
     {
	variable w = width;
	if (w > todo) w = todo;
	variable dlen = len - i;
	if (w > dlen)
	  {
	     ()=fputs (substr (s, i+1, dlen), stdout);
	     w -= dlen;
	     todo -= dlen;
	     i = 0;
	  }
	
	() = fputs (substr(s,i+1,w),stdout);
	i += w;
	() = fputs ("\n", stdout);
	todo -= w;
     }
}

private define make_table ()
{
   variable n = _NARGS/2;
   variable t = struct 
     {
	ch, prob
     };
   t.ch = Char_Type[n];
   t.prob = Double_Type[n];
   _for (n-1, 0, -1)
     {
	variable i = ();
	t.prob[i] = ();
	t.ch[i] = ();
     }
   t.prob = cumsum (t.prob);
   return t;
}

private variable IUB = 
  make_table (('a', 0.27), ('c', 0.12), ('g', 0.12), ('t', 0.27),
	      ('B', 0.02), ('D', 0.02), ('H', 0.02), ('K', 0.02),
	      ('M', 0.02), ('N', 0.02), ('R', 0.02), ('S', 0.02),
	      ('V', 0.02), ('W', 0.02), ('Y', 0.02));
private variable Homosapiens =
  make_table (('a', 0.3029549426680), ('c', 0.1979883004921),
	      ('g', 0.1975473066391), ('t', 0.3015094502008));

private variable ALU = 
   "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG" +
   "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA" +
   "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT" +
   "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA" +
   "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG" +
   "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC" +
   "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA";

define slsh_main ()
{
   variable n = 1000;
   if (__argc > 1) n = integer (__argv[1]);
   make_repeat_fasta ("ONE","Homo sapiens alu",ALU,n*2);
   make_random_fasta ("TWO", "IUB ambiguity codes",IUB,n*3);
   make_random_fasta ("THREE","Homo sapiens frequency",Homosapiens,n*5);
}
define fib ();
define fib(n)
{
   if (n < 2)
     return 1;
   fib(n-2) + fib(n-1);
}
define main ()
{
   variable n = 1;
   if (__argc > 1) n = integer (__argv[1]);
   fprintf (stdout, "%d\n", fib(n));
}
main ();
#!/usr/bin/env slsh
% The Great Computer Language Shootout
% http://shootout.alioth.debian.org/
% 
% contributed by John E Davis

define slsh_main ()
{
   variable n = 10000000, dn = 4096, s=0.0;
   if (__argc > 1) n = integer (__argv[1]);
   while (n > dn)
   {
      variable n0 = n-dn;
      s += sum (1.0/[n0:n]);
      n = n0-1;
   }
   s += sum (1.0/[1:n]);
   ()=fprintf (stdout, "%.9f\n", s);
}
define main ()
{
   variable n=1;
   if (__argc > 1) n = integer (__argv[1]);
   variable X = Assoc_Type[Int_Type];
   _for (1, n, 1)
     {
	variable i = ();
	X[sprintf("%x", i)] = i;
     }
   variable c = 0;
   _for (n, 1, -1)
     {
	i = string ();
	if (assoc_key_exists (X, i))
	  c++;
     }
   fprintf (stdout, "%d\n", c);
}

main();
% -*- mode: slang -*-
% $Id: hash2.slang,v 1.2 2004-11-23 08:08:43 bfulgham Exp $
% http://shootout.alioth.debian.org/
%
% Provided by John E. Davis
%

define main ()
{
   variable n = 1;
   if (__argc > 1) n = integer(__argv[1]);
   variable hash1 = Assoc_Type[Int_Type];
   _for (1, 10000, 1)
     {
	variable i = ();
	hash1[strcat ("foo_", string(i))] = i;
     }

   variable hash2 = Assoc_Type[Int_Type, 0];
   loop (n)
     {
	foreach (hash1) using ("keys", "values")
	  {
	     variable k, v;
	     (k,v) = ();
	     hash2[k] += v;
	  }
     }

   fprintf (stdout, "%u %u %u %u\n",
	     hash1["foo_1"], hash1["foo_9999"], 
	     hash2["foo_1"], hash2["foo_9999"]);
}

main();


variable IM = 139968;
variable IA =   3877;
variable IC =  29573;

variable LAST = 42;
define gen_random(max)
{
   LAST = (LAST * IA + IC) mod IM;
   return (max * LAST) / IM;
}

define heapsort(n, ra)
{
   variable rra = 0, i = 0, j = 0;
   variable l = (n shr 1) + 1;
   variable ir = n;

   forever
     {
        if (l > 1)
	  {
	     l--;
	     rra = ra[l];
	  }
        else
	  {
	     rra = ra[ir];
	     ra[ir] = ra[1];
	     if (ir == 1)
	       {
		  ra[1] = rra;
		  return;
	       }
	     ir--;
	  }
        i = l;
        j = l*2;
	forever 
	  {
	     if (j > ir)
	       break;

	     variable raj = ra[j];
	     if (j < ir)
	       {
		  variable raj1 = ra[j+1];
		  if (raj < raj1)
		    {
		       j++;
		       raj=raj1;
		    }
	       }
	     if (rra >= raj)
	       {
		  j = ir + 1;
		  break;
	       }

	     ra[i] = raj;
	     i = j;
	     j *= 2;
	  }

        ra[i] = rra;
     }
}


define main()
{
   variable N = 1;
   if (__argc > 1) N = integer (__argv[1]);
   if (N < 1)
     N = 1;
   variable ary = array_map (Double_Type, &gen_random, [0:N]*0+1.0);
   heapsort(N, ary);

   fprintf (stdout, "%.10f\n", ary[N]);
}


main();
fprintf(stdout, "hello world\n");
define new_list (n)
{
   variable l = struct
     {
	root, tail, data, len
     };
   l.data = [1:n];
   l.root = 0;
   l.tail = n;
   l.len = n;
   return l;
}

define expand_list (l, len)
{
   len += 4096;
   variable data = Int_Type[len];
   variable root = l.root;
   variable tail = l.tail;
   variable n = (tail - root);
   if (n)
     data[[0:n-1]] = l.data[[root:tail-1]];
   l.data = data;
   l.root = 0;
   l.tail = n;
   l.len = len;
}

define list_append (l, value)
{
   variable tail = l.tail;

   if (l.tail == l.len)
     {
	expand_list (l, l.len + 1);
	tail = l.tail;
     }

   l.data[tail] = value;
   tail++;
   l.tail = tail;
}

define list_pop_right (l)
{
   variable tail = l.tail;
   if (tail == l.root)
     return NULL;
   tail--;
   l.tail = tail;
   return l.data[tail];
}

define list_pop_left (l)
{
   variable root = l.root;
   if (l.tail == root)
     return NULL;
   l.root = root+1;
   return l.data[root];
}

define list_length (l)
{
   return l.tail - l.root;
}

define reverse_list (l)
{
   variable tail = l.tail;
   variable root = l.root;
   if (tail == root)
     return;

   tail--;
   l.data[[root:tail]] = l.data[[tail:root:-1]];
}

define dup_list (l)
{
   variable new_l = @l;
   new_l.data = @l.data;
   return new_l;
}

define list_to_array (a)
{
   variable tail, root;
   tail = a.tail;
   root = a.root;
   if (root == tail)
     return NULL;
   tail--;
   return a.data[[root:tail]];
}

define check_eqs (a, b)
{
   if (list_length (a) != list_length (b))
     return 0;
   variable data_a = list_to_array (a);
   variable data_b = list_to_array (b);
   if (data_a == NULL)
     return 1;			       %  same length, but empty
   
   return not length (where (data_a != data_b));
}

variable SIZE = 10000;	
define test_lists ()
{
   variable L1 = new_list (SIZE);
   variable L2 = dup_list (L1);
   variable L3 = new_list (0);
   
   forever 
     {
	variable node = list_pop_left (L2);
	if (node == NULL)
	  break;

	list_append (L3, node);
     }

   forever 
     {
	node = list_pop_right (L3);
	if (node == NULL)
	  break;
	
	list_append (L2, node);
     }
   reverse_list (L1);

   if (L1.data[L1.root] != SIZE)
     return -1;

   if (0 == check_eqs (L1, L2))
     return -2;
   
   return list_length (L1);
}

    
define main ()
{
   variable num = 1;
   if (__argc > 1) num = integer (__argv[1]);
   loop (num)
     num = test_lists ();
   
   fprintf (stdout, "%d\n", num);
}

main ();
   
#!/usr/bin/env slsh
% The Computer Language Shootout Benchmarks
% http://shootout.alioth.debian.org/
% 
% contributed by John E. Davis

define slsh_main ()
{
   variable w = 100;
   if (__argc == 2) w = integer (__argv[1]);

   variable h = w, iter = 50;
   variable byte_acc = 0, bit_num = 0;
   variable limit = 2.0;

   () = fprintf (stdout, "P4\n%d %d\n", w, h);

   _for (0, h-1, 1)
     {
	variable y = ();
	variable x = [0:w-1];
	variable z = Complex_Type[w];
	variable c = ((2.0*x)/w + ((2i*y)/h - 1i - 1.5));
	variable i;
	loop (iter)
	  {
	     z = z*z + c;
	     i = where (abs(z) < limit);
	     z = z[i]; c = c[i]; x = x[i];
	  }
	variable bytes = UChar_Type[(w-1)/8+1];
	variable bits = (1 shl (7 - (x mod 8)));
	x /= 8;
	_for (0, length (x)-1, 1)
	  {
	     i = ();
	     bytes[x[i]] |= bits[i];
	  }
	() = fwrite (bytes, stdout);
     }
}
variable size = 30;

define mkmatrix(rows, cols)
{
   variable mx = [1:rows*cols];
   reshape (mx, [rows, cols]);
   return mx;
}

define main()
{
   variable iter = 1;
   if (__argc > 1) iter = integer (__argv[1]);
   variable m1 = mkmatrix(size, size);
   variable m2 = mkmatrix(size, size);
   
   loop (iter)
     variable mm = m1 # m2;

   fprintf (stdout, "%.10g %.10g %.10g %.10g\n", mm[0,0], mm[2,3], mm[3,2], mm[4,4]);
}

main ();
variable Toggle = struct { state, value, activate };

define toggle_value (this) 
{
   return this.state;
}

define toggle_activate (this)
{
   this.state = not this.state;
   return this;
}

define init_Toggle (this, start_state) 
{
   this.state = start_state;
   this.value = &toggle_value;
   this.activate = &toggle_activate;
   return this;
}
define new_Toggle (start_state) 
{
   return init_Toggle (@Toggle, start_state);
}

define NthToggle_activate (this)
{
   variable counter = this.counter;
   counter++;
   if (this.count_max <= counter)
     {
	this.state = not (this.state);
	counter = 0;
    }
   this.counter = counter;
   return this;
}

define init_NthToggle (this, max_count) 
{
   this.count_max = max_count;
   this.counter = 0;
   this.activate = &NthToggle_activate;
   return this;
}

define new_NthToggle (start_state, max_count) 
{
   variable this = @Struct_Type 
     ([get_struct_field_names(Toggle), "count_max", "counter"]);
   this = init_Toggle(this, start_state);
   return init_NthToggle(this, max_count);
}

define do_toggle (tog, n)
{
   variable val;
   loop (n)
     val = @((@tog.activate)(tog).value)(tog);
   if (val)
     fprintf (stdout, "true\n");
   else
     fprintf (stdout, "false\n");
}

define main ()
{
   variable n = 1;
   if (__argc == 2) n = integer (__argv[1]);

   do_toggle (new_Toggle (1), n);
   do_toggle (new_NthToggle (1, 3), n);
}
main();
define main ()
{
   variable nums = array_map (Double_Type, &atof, fgetslines (stdin));
   variable n = length (nums);
   variable sum_nums = sum (nums);
   variable mean = sum_nums/n;

   variable deviation = nums - mean;
   variable average_deviation = sum (abs(deviation))/n;
   variable variance = sum (deviation^2)/(n-1);
   variable skew = sum (deviation^3);
   variable kurtosis = sum (deviation^4);
   variable standard_deviation = sqrt(variance);
     
   if (variance > 0.0)
     {
        skew /= (n * variance * standard_deviation);
        kurtosis = kurtosis/(n * variance * variance) - 3.0;
     }

   nums = nums[array_sort(nums)];
   variable mid = n/2;
   variable median;

    if (n mod 2)
     median = nums[mid];
   else
     median = (nums[mid] + nums[mid-1])/2;

   fprintf (stdout, "n:                  %d\n", n);
   fprintf (stdout, "median:             %f\n", median);
   fprintf (stdout, "mean:               %f\n", mean);
   fprintf (stdout, "average_deviation:  %f\n", average_deviation);
   fprintf (stdout, "standard_deviation: %f\n", standard_deviation);
   fprintf (stdout, "variance:           %f\n", variance);
   fprintf (stdout, "skew:               %f\n", skew);
   fprintf (stdout, "kurtosis:           %f\n", kurtosis);
}
main ();
#!/usr/bin/env slsh
% The Great Computer Language Shootout
% http://shootout.alioth.debian.org/
% 
% contributed by John E Davis

private variable SOLAR_MASS = 4 * PI * PI;
private variable DAYS_PER_YEAR = 365.24;

typedef struct
{
   X, V, mass
}
Planet;

private define advance (bodies, dt)
{
   variable i, j;
   variable nbodies = length (bodies);
   _for (0, nbodies-1, 1)
     {
	i = ();
	variable b = bodies[i];
	variable X = b.X, V = b.V;
	variable x = X[0], y = X[1], z = X[2];
	variable vx = V[0], vy = V[1], vz = V[2];
	variable mass = b.mass;
	_for (i+1, nbodies-1, 1)
	  {
	     j = ();
	     variable b1 = bodies[j];
	     variable X1 = b1.X;
	     variable V1 = b1.V;
	     variable dx = x - X1[0];
	     variable dy = y - X1[1];
	     variable dz = z - X1[2];
	     variable mag = dt/(dx*dx+dy*dy+dz*dz)^1.5;
	     variable mag1 = b1.mass * mag;
	     vx = vx-dx * mag1;
	     vy = vy-dy * mag1;
	     vz = vz-dz * mag1;
	     mag1 = mag*mass;
	     V1[0] = V1[0] + (dx*mag1);
	     V1[1] = V1[1] + (dy*mag1);
	     V1[2] = V1[2] + (dz*mag1);
	  }
	V[0] = vx; V[1] = vy; V[2] = vz;
	X[0] = x + vx*dt;
	X[1] = y + vy*dt;
	X[2] = z + vz*dt;
     }
}

private define energy (bodies)
{
   variable nbodies = length (bodies);
   variable e = 0.0, i, j;
   _for (0, nbodies-1, 1)
     {
	i = ();
	variable b = bodies[i];
	variable X = b.X;
	variable V = b.V;
	variable mass = b.mass;
	e += 0.5 * mass * sum (V*V);
	_for (i+1, nbodies-1, 1)
	  {
	     j = ();
	     variable b_j = bodies[j];
	     variable dX = X-b_j.X;
	     e -= (mass * b_j.mass)/sqrt(sum(dX*dX));
	  }
     }
   return e;
}

private define offset_momentum (bodies)
{
   variable i, p = 0.0;
   _for (0, length(bodies)-1, 1)
     {
	i = ();
	variable b = bodies[i];
	p += b.mass * b.V;
     }
   bodies[0].V = -p/SOLAR_MASS;
}

private define make_planet (X, V, mass)
{
   variable p = @Planet;
   p.X = X;
   p.V = V * DAYS_PER_YEAR;
   p.mass = mass * SOLAR_MASS;
   return p;
}

private variable Jupiter =
  make_planet ([4.84143144246472090e+00,-1.16032004402742839e+00,-1.03622044471123109e-01],
	       [1.66007664274403694e-03, 7.69901118419740425e-03,-6.90460016972063023e-05],
	       9.54791938424326609e-04);
private variable Saturn =
  make_planet ([8.34336671824457987e+00,4.12479856412430479e+00,-4.03523417114321381e-01],
	       [-2.76742510726862411e-03,4.99852801234917238e-03,2.30417297573763929e-05],
	       2.85885980666130812e-04);
private variable Uranus =
  make_planet ([1.28943695621391310e+01,-1.51111514016986312e+01,-2.23307578892655734e-01],
	       [2.96460137564761618e-03, 2.37847173959480950e-03,-2.96589568540237556e-05],
	       4.36624404335156298e-05);

private variable Neptune =
  make_planet ([1.53796971148509165e+01,-2.59193146099879641e+01,1.79258772950371181e-01],
	       [2.68067772490389322e-03, 1.62824170038242295e-03,-9.51592254519715870e-05],
	       5.15138902046611451e-05);
private variable Sun = make_planet ([0.0,0.0,0.0], [0.0,0.0,0.0], 1.0);

define slsh_main ()
{
   variable n = 1000;
   if (__argc == 2)
     n = integer (__argv[1]);

   variable bodies = [Sun, Jupiter, Saturn, Uranus, Neptune];
   offset_momentum (bodies);
   () = fprintf (stdout, "%.9f\n", energy(bodies));
   loop (n)
     advance (bodies, 0.01);
   () = fprintf (stdout, "%.9f\n", energy(bodies));
}
#!/usr/bin/env slsh
% The Computer Language Shootout Benchmarks
% http://shootout.alioth.debian.org/
%
% contributed by John E. Davis
private variable SOLAR_MASS = 4 * PI * PI;
private variable DAYS_PER_YEAR = 365.24;

typedef struct { X, V, mass } Planet;

private define advance_n (num, bodies, dt)
{
   variable X = bodies.X, V = bodies.V;
   variable n3 = length (V), n = n3/3;
   variable i = [0:n*n-1]/n, j = [0:n*n-1] mod n, k = [3,n,n];
   variable I = [i, i+n, i+2*n]; reshape (I, k);
   variable J = [j, j+n, j+2*n]; reshape (J, k);
   variable mass_dt = dt * bodies.mass[_reshape(i,[n,n])];
   variable expon = Double_Type[n,n] - 1.5; expon[[::n+1]] = 0.0;
   loop (num)
     {
	variable dX = X[I] - X[J];
	variable a = mass_dt*(sum(dX*dX,0))^expon;
	a = [a, a, a]; reshape (a, k);
	V = V + sum (a*dX, 1);
	X = X + V*dt;
     }
   bodies.V = V; bodies.X = X;
}


private define energy (bodies)
{
   variable mass = bodies.mass,  nbodies = length (mass);
   variable V = bodies.V, X = bodies.X;
   variable x = X[0,*], y = X[1,*], z = X[2,*];
   variable e = 0.5 * sum (mass * sum (V*V, 0));
   _for (0, nbodies-1, 1)
     {
	variable i = ();
	variable j = [i+1:];
	e -= sum((mass[i]*mass[j])/sqrt((x[i]-x[j])^2+(y[i]-y[j])^2+(z[i]-z[j])^2));
     }
   return e;
}

private define offset_momentum (bodies)
{
   bodies.V[*,0] = -bodies.V#(bodies.mass/SOLAR_MASS);
}

private define make_planet (X, V, mass)
{
   variable p = @Planet;
   p.X = X;
   p.V = V * DAYS_PER_YEAR;
   p.mass = mass * SOLAR_MASS;
   return p;
}

private variable Jupiter =
  make_planet ([4.84143144246472090e+00,-1.16032004402742839e+00,-1.03622044471123109e-01],
	       [1.66007664274403694e-03, 7.69901118419740425e-03,-6.90460016972063023e-05],
	       9.54791938424326609e-04);
private variable Saturn =
  make_planet ([8.34336671824457987e+00,4.12479856412430479e+00,-4.03523417114321381e-01],
	       [-2.76742510726862411e-03,4.99852801234917238e-03,2.30417297573763929e-05],
	       2.85885980666130812e-04);
private variable Uranus =
  make_planet ([1.28943695621391310e+01,-1.51111514016986312e+01,-2.23307578892655734e-01],
	       [2.96460137564761618e-03, 2.37847173959480950e-03,-2.96589568540237556e-05],
	       4.36624404335156298e-05);

private variable Neptune =
  make_planet ([1.53796971148509165e+01,-2.59193146099879641e+01,1.79258772950371181e-01],
	       [2.68067772490389322e-03, 1.62824170038242295e-03,-9.51592254519715870e-05],
	       5.15138902046611451e-05);
private variable Sun = make_planet ([0.0,0.0,0.0], [0.0,0.0,0.0], 1.0);

private define pack_bodies (bodies)
{
   variable s = @Planet;
   variable i, n = length (bodies);
   s.X = Double_Type[3,n]; s.V = Double_Type[3,n]; s.mass = Double_Type[n];
   _for i (0, n-1, 1)
     {
	variable b = bodies[i];
	s.X[*,i] = b.X; s.V[*,i] = b.V;
	s.mass[i] = b.mass;
     }
   return s;
}

define slsh_main ()
{
   variable n = 1000;
   if (__argc == 2)
     n = integer (__argv[1]);

   variable bodies = pack_bodies ([Sun, Jupiter, Saturn, Uranus, Neptune]);
   offset_momentum (bodies);
   () = fprintf (stdout, "%.9f\n", energy(bodies));
   advance_n (n, bodies, 0.01);
   () = fprintf (stdout, "%.9f\n", energy(bodies));
}
define main()
{
   variable x = 0;
   variable iter = 1;
   if (__argc > 1) iter = integer (__argv[1]);
   
   loop (iter)
     loop (iter)
       loop (iter)
	 loop (iter)
	   loop (iter)
	     loop (iter)
	       x += 1;
   
   fprintf (stdout, "%d\n", x);
}
main();
#!/usr/bin/env slsh
% The Computer Language Shootout Benchmarks
% http://shootout.alioth.debian.org/
%
% contributed by John E. Davis

private define nsieve (m, isprime)
{
   variable count = 0;
   variable i = 2;
   while (i <= m)
     {
	i = wherefirst (isprime, i);
	if (i == NULL)
	  break;

	isprime[[2*i::i]] = 0;
	i++;
	count++;
     }
   return count;
}

public define slsh_main ()
{
   variable n = 2;
   if (__argc > 1) n = integer (__argv[1]);
   if (n < 2) n = 2;
   variable m, flags;

   foreach m (10000 * (1 shl [n, n-1, n-2]))
     {
	flags = not (UChar_Type[m+1]);
	vmessage ("Primes up to %8d %8d", m, nsieve(m, flags));
     }
   return 0;
}

#!/usr/bin/env slsh
% The Computer Language Shootout Benchmarks
% http://shootout.alioth.debian.org/
%
% contributed by John E. Davis
private define nsieve(m)
{
   variable i, j, k, l, count, flags;
   count = 0; m--; flags = UChar_Type[(m+8)/8];
   _for (2, 7, 1)
     {
	i = (); j = i/8; 
	if (flags[j] & (1 shl (i mod 8))) continue;
	i = [2*i:m:i]; j = i/8;	i = (1 shl (i mod 8));
	k = [0:length(i)-9:8];
	loop (8)
	  {
	     l = j[k]; flags[l] = flags[l] | i[k];
	     k++;
	  }
	count++;
     }
   _for (8,m,1) 
     {
	i = ();
	if (flags[i/8] & (1 shl (i mod 8)))
	  continue;

	count++;
	if (i*4 < m)
	  {
	     i = [2*i:m:i]; j = i/8;
	     flags[j] = flags[j] | (1 shl (i mod 8));
	     continue;
	  }
	_for (2*i, m, i)
	  {
	     i = (); j = i/8;
	     flags[j] = flags[j] | (1 shl (i mod 8));
	  }
     }
   return count;
}

define slsh_main ()
{
   variable n = 2; if (__argc > 1) n = integer (__argv[1]);
   if (n < 2) n = 2;
   variable m, i, flags;

   _for (0, 2, 1)
     {
	i = ();
	m = (1 shl (n-i)) * 10000;
	() = fprintf (stdout, "Primes up to %8d %8d\n", m, nsieve(m));
     }
}

variable Toggle = struct { state, value, activate };

define toggle_value (this) 
{
   return this.state;
}

define toggle_activate (this)
{
   this.state = not this.state;
   return this;
}

define init_Toggle (this, start_state) 
{
   this.state = start_state;
   this.value = &toggle_value;
   this.activate = &toggle_activate;
   return this;
}
define new_Toggle (start_state) 
{
   return init_Toggle (@Toggle, start_state);
}

define NthToggle_activate (this)
{
   variable counter = this.counter;
   counter++;
   if (this.count_max <= counter)
     {
	this.state = not (this.state);
	counter = 0;
    }
   this.counter = counter;
   return this;
}

define init_NthToggle (this, max_count) 
{
   this.count_max = max_count;
   this.counter = 0;
   this.activate = &NthToggle_activate;
   return this;
}

variable NthToggle = @Struct_Type
  ([get_struct_field_names(Toggle), "count_max", "counter"]);

define new_NthToggle (start_state, max_count) 
{
   variable this = init_Toggle(@NthToggle, start_state);
   return init_NthToggle(this, max_count);
}

define do_toggle (tog, n)
{
   variable val;
   loop (n)
     val = @((@tog.activate)(tog).value)(tog);
}

define main ()
{
   variable n = 1;
   if (__argc == 2) n = integer (__argv[1]);
   
   variable tog, ntog, val;
   tog = new_Toggle(1);
   loop (5)
     {
	val = @((@tog.activate)(tog).value)(tog);
	if (val)
	  fprintf (stdout, "true\n");
	else
	  fprintf (stdout, "false\n");
     }
   
   loop (n)
     tog = new_Toggle(1);
    
   () = fputs("\n", stdout);

   ntog = new_NthToggle(1, 3);
   loop (8)
     {
	val = @((@ntog.activate)(ntog).value)(ntog);
	if (val)
	  fprintf (stdout, "true\n");
	else
	  fprintf (stdout, "false\n");
     }
   loop (n)
     ntog = new_NthToggle(1, 3);
}
main ();
#!/usr/bin/env slsh
% The Computer Language Shootout Benchmarks
% http://shootout.alioth.debian.org/
%
% contributed by John E. Davis

define slsh_main ()
{
   variable n = 25000; if (__argc > 1) n = integer (__argv[1]);
   variable s0=1.0,s1=0.0,s2=0.0,s3=0.0,s4=0.0,s5=0.0,s6=0.0,s7=0.0,s8=0.0;
   variable k = 1, dn = 4096;
   variable sgn = 1[Int_Type[dn]]; sgn[[1::2]] = -1;
   while (k <= n)
     {
	variable k1 = k + dn;
	if (k1 > n) 
	  {
	     k1 = n + 1;
	     sgn = sgn[[:n-k]];
	  }
	variable kd = [k:k1-1]*1.0, kd2 = kd*kd, kd3 = kd*kd2;
	s0 += sum ((2.0/3.0)^kd);
	s1 += sum (1.0/sqrt(kd));
	s2 += sum (1.0/(kd2+kd));
	variable sk = sin(kd);
	s3 += sum (1.0/(kd3*sk*sk));
	variable ck = cos(kd);
	s4 += sum (1.0/(kd3*ck*ck));
	s5 += sum (1.0/kd);
	s6 += sum (1.0/kd2);
	s7 += sum (sgn/kd);
	s8 += sum (sgn/(2.0*__tmp(kd)-1.0));
	k = k1;
     }
   () = printf("%.9f\t(2/3)^k\n", s0);
   () = printf("%.9f\tk^-0.5\n", s1);
   () = printf("%.9f\t1/k(k+1)\n", s2);
   () = printf("%.9f\tFlint Hills\n", s3);
   () = printf("%.9f\tCookson Hills\n", s4);
   () = printf("%.9f\tHarmonic\n", s5);
   () = printf("%.9f\tRiemann Zeta\n", s6);
   () = printf("%.9f\tAlternating Harmonic\n", s7);
   () = printf("%.9f\tGregory\n", s8);
}

variable IM = 139968;
variable IA = 3877;
variable IC = 29573;
variable LAST = 42;

define gen_random(max)
{
    LAST = (LAST * IA + IC) mod IM;
    (max * LAST) / IM;
}

define main()
{
   variable N=1;
   if (__argc > 1) N = integer (__argv[1]); 
   if (N < 1)
     N = 1;

   loop (N-1)
     () = gen_random (100.0);

   fprintf (stdout, "%.9f\n", gen_random(100.0));
}

main();
#!/usr/bin/env slsh

#ifnexists DOUBLE_EPSILON
variable DOUBLE_EPSILON = 2.22045e-16;
#endif
private variable delta = sqrt (DOUBLE_EPSILON);
#ifnexists _Inf
variable _Inf = -log(0);
#endif
private variable INFINITY = _Inf;

typedef struct { x,y,z } Vec;
private define vector (x,y,z) { variable v=@Vec; v.x=x; v.y=y; v.z=z; return v;}
private define dot (a,b) { return a.x*b.x + a.y*b.y + a.z*b.z; }
private define add (a,b) { return vector (a.x+b.x, a.y+b.y, a.z+b.z);}
private define sub (a,b) { return vector (a.x-b.x, a.y-b.y, a.z-b.z);}
private define scale (a,v) { return vector (a*v.x, a*v.y, a*v.z); }
private define unitise (a) { return scale(1.0 / sqrt(dot(a, a)), a); }
private define filter_vector (v, i) { return vector (v.x[i], v.y[i], v.z[i]); }
private define replicate_vector (n,v)
{
   return vector (Double_Type[n]+v.x,Double_Type[n]+v.y,Double_Type[n]+v.z);
}

typedef struct {orig, dir} Ray_Type;
private define ray (o, d) 
{ 
   variable r = @Ray_Type; r.orig = o; r.dir = d; return r; 
}
private define filter_rays (r, i)
{
   return ray (filter_vector (r.orig, i), filter_vector (r.dir, i));
}

private variable SphereTag = 1;
private variable GroupTag = 2;
typedef struct { center, radius } Sphere_Type;
private define sphere (c, r) 
{
   variable s = @Sphere_Type;s.center = c; s.radius = r; return s;
}

typedef struct { bound, child } Group_Type;
typedef struct { tag, data } Scene_Type;

private define ray_sphere (r, s)
{
   variable v = sub(s.center, r.orig);
   variable b = dot (v, r.dir);
   variable disc = b*b - dot (v,v) + s.radius*s.radius;
   
   variable d = sqrt (disc);
   variable t2 = b + d;
   variable t1 = b - d;
   variable i = where (t1>0.0);
   t2[i] = t1[i];
   t2[where ((disc < 0.0) or (t2 < 0.0))] = INFINITY;
   return t2;
}

private define intersect ();
private define intersect (lambda, normal, r, scene)
{
   variable l, i, j, n;
   EXIT_BLOCK
     {
	lambda[j] = l;
	normal.x[j] = n.x; normal.y[j] = n.y; normal.z[j] = n.z;
     }

   if (scene.tag == SphereTag)
     {
	variable s = scene.data;
	l = ray_sphere (r, s);
	j = where (l < lambda);	r = filter_rays (r, j);	l = l[j];
	n = unitise (add(r.orig, sub (scale (l,r.dir), s.center)));
	return;
     }

   variable g = scene.data;
   l = ray_sphere (r, g.bound);
   j = where (l < lambda);  
   r = filter_rays(r, j); n = filter_vector (normal,j); l = lambda[j];
   foreach (g.child) {g=(); intersect (l, n, r, g);}
}

private define ray_trace (n, light, r, scene)
{
   variable lambda = Double_Type[n]; lambda[*] = INFINITY;
   variable normal = vector(Double_Type[n],Double_Type[n],Double_Type[n]);
   intersect (lambda, normal, r, scene);
   variable g = Double_Type[n], g1 = dot (normal, light);
   variable i = where ((lambda != INFINITY) and (g1 > 0.0));
   g[i] = g1[i];  normal = filter_vector (normal, i); lambda = lambda[i];
   r = filter_rays (r, i);

   variable r2 = ray (add(r.orig,scale(lambda,add(r.dir,scale(delta,normal)))),
		      replicate_vector(length(i),light));
   lambda[*] = INFINITY;
   intersect(lambda, normal, r2, scene);
   g[i[where (lambda != INFINITY)]] = 0;
   return g;
}

private define create ();
private define create (level, r, x, y, z)
{
   variable scene = @Scene_Type;
   if (level == 1)
     {
	scene.tag = SphereTag;
	scene.data = sphere(vector(x, y, z), r);
	return scene;
     }
   
   variable g = @Group_Type;
   scene.tag = GroupTag;
   scene.data = g;
   g.bound = sphere(vector(x, y, z), 3*r);
   variable n = 5;
   variable cs = Scene_Type[n];
   g.child = cs;
   variable  rn = 3*r/sqrt(12.0);
   _for (0, n-2, 1)
     {
	variable i = ();
	cs[i] = create (level-1, r/2.0, 
			x - ((i&1)*2-1)*rn, y + rn, z - ((i/2)*2-1)*rn);
     }
    cs[n-1] = create(1, r, x, y, z);
   return scene;
}

define slsh_main ()
{
   variable n = 256;
   if (__argc > 1) n = integer (__argv[1]);
   variable level = 6, ss=4, n2 = n*n, i;
   variable light = sub(vector(0.0,0.0,0.0),unitise(vector(-1.0, -3.0, 2.0)));
   variable scene = create(level, 1, 0, -1, 0);
   variable x0 = Double_Type[n,n], y0 = Double_Type[n,n], z0 = Double_Type[n,n];
   _for (0, n-1, 1) { i=(); x0[i,*] = [0:n-1]; y0[*,i] = [n-1:0:-1]; }
   reshape (x0, [n2]); reshape (y0, [n2]); reshape (z0, [n2]);
   z0[*] = n;

   variable g = 0.0, dx, dy;
   variable orig = replicate_vector (n2,vector(0.0,0.0,-4.0));
   for (dx=0; dx<ss; ++dx)
     {
        for (dy=0; dy<ss; ++dy) 
	  {
	     variable d = vector (x0 + (dx/double(ss)-0.5*n),
				  y0 + (dy/double(ss)-0.5*n), z0);
	     g += ray_trace (n2, light, ray(orig, unitise(d)), scene);
	  }
    }
   reshape (g, [n,n]);
   ()=printf("P5\n%d %d\n255\n", n, n);
   () = fwrite (typecast (0.5 + (255.0/(ss*ss))*g, UChar_Type), stdout);
}
()=array_map (Int_Type, &fputs, fgetslines (stdin)[[-1:0:-1]], stdout);
define main()
{
   variable NUM;
   if (__argc > 1) NUM = integer (__argv[1]); else NUM = 1;
   variable flags = Int_Type[8192 + 1];
   loop (NUM)
     {
        flags[*] = 1;
        variable count = 0;
	_for (2, 8192, 1)
	  {
	     variable i = ();
	     if (flags[i])
	       {
		  flags[[i+i:8192:i]] = 0;
		  count++;
	       }
	  }
     }
   
   fprintf (stdout, "Count: %d\n", count);
}
main();
#!/usr/bin/env slsh
% The Great Computer Language Shootout
% http://shootout.alioth.debian.org/
%
% contributed by John E Davis

private define eval_A(i,j) { 1.0/((i+j)*(i+j+1)/2+i+1); }

private define eval_A_times_u (n, u, Au)
{
   variable j = [0:n-1];
   _for (0, n-1, 1)
     {
	variable i = ();
	Au[i] = sum (eval_A(i,j)*u);
     }
}

private define eval_At_times_u (n, u, Au)
{
   variable j = [0:n-1];
   _for (0, n-1, 1)
     {
	variable i = ();
	Au[i] = sum (eval_A (j, i)*u);
     }
}

private define eval_AtA_times_u (n, u, AtAu)
{
   variable v = Double_Type[n];
   eval_A_times_u (n,u,v); eval_At_times_u (n,v,AtAu);
}

define slsh_main ()
{
   variable n = 2000;
   if (__argc == 2) n = integer (__argv[1]);
   variable u = 1+Double_Type[n], v = Double_Type[n];
   loop (10)
     {
	eval_AtA_times_u(n,u,v);
	eval_AtA_times_u(n,v,u);
     }
   variable vBv = sum (u*v), vv = sum(v*v);
   () = fprintf (stdout, "%0.9f\n", sqrt(vBv/vv));
}
define main()
{
   variable dict = Assoc_Type[Int_Type, 0];
   foreach (fopen("Usr.Dict.Words", "r")) using ("wsline")
     {
	variable word = ();
	dict[word] = 1;
     }
   foreach (stdin) using ("wsline")
     {
	word = ();
	!if (dict[word])
	  () = fprintf (stdout, "%s\n", word);
     }
}

main();
define main_slow()
{
   variable n = integer (__argv[1]);
   variable str = typecast ("", BString_Type);
   variable hello = typecast ("hello\n", BString_Type);
   loop (n)
     str += hello;
   fprintf (stdout, "%S\n", strlen (str));
}

variable Buffer = struct
{
   size, data, len
};

define new_buffer (s)
{
   variable b = @Buffer;
   s = typecast (s, BString_Type);
   b.data = bstring_to_array (s);
   b.len = bstrlen (s);
   b.size = b.len;
   return b;
}

define append (b, b1)
{
   variable len = b.len;
   variable len1 = b1.len;
   variable new_len = len + len1;
   variable data = b.data;
   variable i;

   if (b.size < new_len)
     {
	b.size = new_len * 2;
	variable new_data = UChar_Type[2*new_len];
	if (len)
	  {
	     i = [0:len-1];
	     new_data[i] = data[i];
	  }

	data = new_data;
	b.data = data;
     }
   
   i = [len:new_len-1];
   data[i] = b1.data[[0:len1-1]];
   b.len = new_len;
}


define main_faster ()
{
   variable n;
   if (__argc > 1) n = integer (__argv[1]); else n = 1;

   variable hello = new_buffer ("hello\n");
   variable str = new_buffer ("");
   loop (n)
     append (str, hello);
   fprintf (stdout, "%S\n", str.len);
}

main_faster ();
define  main()
{
   variable count = 0;
   foreach (stdin) using ("line")
     count += atoi ();
   fprintf (stdout, "%d\n", count);
}
main();
% takfp.slang
%
% Contributed by John E. Davis
%

static define tak ();
static define tak (x, y, z)
{
	if (y >= x) return z;
	return tak(tak(x-1,y,z), tak(y-1,z,x), tak(z-1,x,y));
}

public define slsh_main ()
{
	variable n = 1;
	if (__argc > 1) n = integer (__argv[1]);

	() = fprintf(stdout, "%.1f\n", tak(n*3.0, n*2.0, n*1.0));
	return 0;
}

% vim: ts=4 ft=slang
define main()
{
   variable nl, nw, nc;
   
   nl = 0; nw = 0; nc = 0;
   
   foreach (stdin) using ("line")
     {
	variable line = ();
        nl++;
	nc += strlen (line);
	nw += length (strtok (line));
     }

   fprintf (stdout, "%d %d %d\n", nl, nw, nc);
}

main()
define main()
{
   variable count = Assoc_Type[Int_Type, 0];
   foreach (stdin) using ("line")
     {
	variable line = ();
	foreach (strtok (strlow (line), "^a-z"))
	  {
	     variable word = ();
	     count [word] += 1;
	  }
     }
   
   variable words = assoc_get_keys (count);
   variable freqs = assoc_get_values (count);

   variable i = array_sort (array_map (String_Type, &sprintf, "%12d", freqs)
			    + words);
   i = i[[-1:0:-1]];
   
   foreach (i)
     {
	i = ();
	()=fprintf (stdout, "%7d %s\n", freqs[i], words[i]);
     }
}

main();
