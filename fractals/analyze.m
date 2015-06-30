#!/usr/local/bin/octave

pkg load statistics;

arg_list = argv ();
filename = arg_list{1};
label = arg_list{2};

disp(filename);
t=load(filename);
t=t(:,2);    % extracting just column 2, with the actual line length values
[H,R,cf,S,F, msg]=dfab(2,t,10,1200,1,label);

myfile=fopen("analysis.txt","w") % Open the file 
fprintf(myfile,"%s\n", msg) 
fclose(myfile)

 