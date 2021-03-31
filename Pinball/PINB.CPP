#include<iostream.h>
#include<graphics.h>
#include<dos.h>
#include<conio.h>
#include<process.h>
#include<alloc.h>
#include<stdlib.h>
#include<math.h>
#include<stdio.h>
#include<fstream.h>
#include<string.h>

  int n=12,on;
  int cols[]={1,12,13,14,15};
  REGS i,o;
  int chance;
  static float dx,dy,v;
  int x=320,y=23,ox,oy,check;
  long score;
  int fl1x[5][10]={{250,405,250,399,285,416,285,419,250,405},
		  {250,405,250,399,289,408,289,411,250,405 },
		  {250,405,250,399,290,400,290,403,250,405 },
		  {250,405,250,399,289,392,289,395,250,405 },
		  {250,405,250,399,285,384,285,387,250,405}};
  int fl2x[5][10]={{349,405,349,399,314,416,314,419,349,405},
		  {349,405,349,399,310,408,310,411,349,405 },
		  {349,405,349,399,309,400,309,403,349,405 },
		  {349,405,349,399,310,392,310,395,349,405 },
		  {349,405,349,399,314,384,314,387,349,405}};
  int st[2],ost[2];
  int X[]={1,3,4,5,6,6,6,6,5,4,3,1,-1,-3,-4,-5,-6,-6,-6,-6,-5,-4,-3,-1};
  int Y[]={-6,-6,-5,-4,-3,-1,1,3,4,5,6,6,6,6,5,4,3,1,-1,-3,-4,-5,-6,-6};
  int X1[]={1,3,4,5,6,7,7,6,5,4,3,1,-1,-3,-4,-5,-6,-7,-7,-6,-5,-4,-3,-1};
  int Y1[]={-7,-6,-5,-4,-3,-1,1,3,4,5,6,7,7,6,5,4,3,1,-1,-3,-4,-5,-6,-7};

void flap(int t)
{
ost[0]=st[0];
ost[1]=st[1];
if(t==3)
{
if(st[0]<4)
   {
    st[0]+=2;
   // dy=-4;
   }
if(st[0]==4)
    st[0]++;
if(st[1]<4)
   {
    st[1]+=2;
   // dy=-4;
   }
if(st[1]==4)
    st[1]++;
}
else{
if((st[t-1]<4)&&(t>0)&&(t<3))
   {
    st[t-1]+=2;
   // dy=-4;
   }
if((st[t-1]==4)&&(t>0))
	st[t-1]++;
}

if(st[0]>0)
{   st[0]--;}

if(st[1]>0)
{
    st[1]--;
}
 setfillstyle(1,on);
 setlinestyle(0,1,3);
 setcolor(on);
 setcolor(on);
 fillpoly(4,fl1x[ost[0]]);
 fillpoly(4,fl2x[ost[1]]);
 setcolor(8);
 setfillstyle(1,8);
 fillpoly(4,fl1x[st[0]]);
 fillpoly(4,fl2x[st[1]]);
 setcolor(4);
 setlinestyle(SOLID_LINE,1,0);
}

void cal1(int num,int col,int fn)
{
if((col==3)||(col==8)||(col==11)||(col==10))
{
score+=3;
dx-=(X1[num]);
dy-=(Y1[num]);
if(col==8)
{
if(((fn==1)&&(st[0]>0)&&(st[0]<4)&&(x<300))||((fn==2)&&(st[1]>0)&&(st[1]<4)&&(x>300)))
{dy=-40;y-=20;v=.08; }
else if((st[0]==0)||(st[0]==4)||(st[1]==0)||(st[1]==4))
{dx=.9*dx;dy=.9*dy; }
}
if(col==11)
{
   setcolor(11);
   setfillstyle(1,10);
   circle(265,105,10);
   floodfill(265,105,11);
   circle(330,110,10);
   floodfill(330,110,11);
   circle(295,145,10);
   floodfill(295,145,11);
	delay(7);
	score+=145;
	setfillstyle(1,6);
	floodfill(265,105,11);
	floodfill(330,110,11);
	floodfill(295,145,11);
}
if(col==10)
{
   setfillstyle(1,11);
   setcolor(10);
   circle(206,45,10);
   floodfill(206,45,10);
	delay(7);
	score+=595;
	setfillstyle(1,3);
	setcolor(10);
	circle(206,45,10);
	floodfill(206,45,10);
}
}
if(col==3)
{
}

}

void cal(int num)
{
dx-=(X[num]);
dy-=(Y[num]);
}

void dis(char*a,char*b)
{
   setfillstyle(1,on+2);
   setcolor(on+4);
   int c[]={448,150,639,150,639,350,448,350,448,150};
   fillpoly(4,c);
   settextstyle(4,0,4);
   outtextxy(453,200,a);
   outtextxy(463,250,b);
}

void game(void)
{

  setlinestyle(SOLID_LINE,1,0);
  int x=433,y=353,ox,oy,check;
  int gp[24];
  st[0]=0;
  st[1]=0;
  ost[0]=0;
  ost[1]=0;
  dx=0;
  dy=-6;
  v=.1;
  int cont=0;
  while(!kbhit())
  {
     cont++;
   //   cout<<cont;
     if(cont==111)
     {break;}
     ox=x;
     oy=y;
     check=0;
     for(int t=0;t<24;t++)
     {
	gp[t]=getpixel(x+X[t],y+Y[t]);
	if(gp[t]!=1)
	   check++;
     }


//BALL LOGIC

     if(gp[0]==3)
	cal(0);
     if(gp[1]==3)
	cal(1);
     if(gp[2]==3)
	cal(2);
     if(gp[3]==3)
	cal(3);
     if(gp[4]==3)
	cal(4);
     if(gp[5]==3)
	cal(5);
     if(gp[6]==3)
	cal(6);
     if(gp[7]==3)
	cal(7);
     if(gp[8]==3)
	cal(8);
     if(gp[9]==3)
	cal(9);
     if(gp[10]==3)
	cal(10);
     if(gp[11]==3)
	cal(11);
     if(gp[12]==3)
	cal(12);
     if(gp[13]==3)
	cal(13);
     if(gp[14]==3)
	cal(14);
     if(gp[15]==3)
	cal(15);
     if(gp[16]==3)
	cal(16);
     if(gp[17]==3)
	cal(17);
     if(gp[18]==3)
	cal(18);
     if(gp[19]==3)
	cal(19);
     if(gp[20]==3)
	cal(20);
     if(gp[21]==3)
	cal(21);
     if(gp[22]==3)
	cal(22);
     if(gp[23]==3)
	cal(23);
     if(gp[24]==3)
	cal(24);
 /*
     if(gp[25]==3)
	cal(25);
     if(gp[26]==3)
	cal(26);
     if(gp[27]==3)
	cal(27);
     if(gp[28]==3)
	cal(28);
   */
//BALL LOGIC

     x=x+(dx*v);
     y=y+(dy*v);


     setfillstyle(1,on);
     setcolor(on);
     fillellipse(ox,oy,4,4);
     setfillstyle(1,4);
     setcolor(4);
     fillellipse(x,y,4,4);
     delay(8);
  }

}
void game1(void)
{
   setcolor(4);
   setlinestyle(1,0,3);
   line(420,116,433,112);
   line(422,119,435,115);
  setlinestyle(SOLID_LINE,1,0);
  x=320;
  y=23;
  int gp[24];
  randomize();
  randomize();
  dx=-(random(7));
  randomize();
  dy=random(2);
  v=.08;
  int w;
  while(!kbhit())
  {

     gotoxy(5,15);
     cout<<"SCORE : ";
     gotoxy(10,16);
     cout<<"        ";
     gotoxy(10,16);
     cout<<score;

     w=0;
     i.x.ax=3;
     int86(0X33,&i,&o);
     w=o.x.bx;
     if((w>0)||st[0]!=0||st[1]!=0)
	flap(w);
     ox=x;
     oy=y;
     check=0;
     for(int t=0;t<24;t++)
     {
	gp[t]=getpixel(x+X1[t],y+Y1[t]);
      //   cout<<gp[t];
      //   getch();
	if(gp[t]!=1)
	   check++;
     }


//BALL LOGIC

     if(gp[0]!=1)
	cal1(0,gp[0],w);
     if(gp[1]!=1)
	cal1(1,gp[1],w);
     if(gp[2]!=1)
	cal1(2,gp[2],w);
     if(gp[3]!=1)
	cal1(3,gp[3],w);
     if(gp[4]!=1)
	cal1(4,gp[4],w);
     if(gp[5]!=1)
	cal1(5,gp[5],w);
     if(gp[6]!=1)
	cal1(6,gp[6],w);
     if(gp[7]!=1)
	cal1(7,gp[7],w);
     if(gp[8]!=1)
	cal1(8,gp[8],w);
     if(gp[9]!=1)
	cal1(9,gp[9],w);
     if(gp[10]!=1)
	cal1(10,gp[10],w);
     if(gp[11]!=1)
	cal1(11,gp[11],w);
     if(gp[12]!=1)
	cal1(12,gp[12],w);
     if(gp[13]!=1)
	cal1(13,gp[13],w);
     if(gp[14]!=1)
	cal1(14,gp[14],w);
     if(gp[15]!=1)
	cal1(15,gp[15],w);
     if(gp[16]!=1)
	cal1(16,gp[16],w);
     if(gp[17]!=1)
	cal1(17,gp[17],w);
     if(gp[18]!=1)
	cal1(18,gp[18],w);
     if(gp[19]!=1)
	cal1(19,gp[19],w);
     if(gp[20]!=1)
	cal1(20,gp[20],w);
     if(gp[21]!=1)
	cal1(21,gp[21],w);
     if(gp[22]!=1)
	cal1(22,gp[22],w);
     if(gp[23]!=1)
	cal1(23,gp[23],w);
     if(gp[24]!=1)
	cal1(24,gp[24],w);
/*     if(gp[25]==3)
	cal(25);
     if(gp[26]==3)
	cal(26);
     if(gp[27]==3)
	cal(27);
     if(gp[28]==3)
	cal(28);
*/


//BALL LOGIC

       if(dx>37)
	  dx=37;
       if(dy>37)
	  dy=37;
       if(dy<-40)
	  dy=-40;
       if(dx<-37)
	  dx=-37
	  ;
  //     if((dx==0)&&(dy==0))
    //	dy=-1;

     x=x+(dx*v);
     y=y+(dy*v);
     /*
     if(((ox-x)>1))
	x++;
     if(((x-ox)>1))
	x--;
     if(((oy-y)>1))
	y++;
     if(((y-oy)>1))
	y--;    */


    if(y>440)
	break;
    // line(255,410,345,410);
     setfillstyle(1,on);
     setcolor(on);
     fillellipse(ox,oy,4,4);
     setfillstyle(1,4);

     if((x>429)&&(y>130))
    {
	chance++;
	dis("Re - Deploy","");
	delay(2000);
	break;
    }
     setcolor(4);
     fillellipse(x,y,4,4);
     dy=dy+(.5);
     delay(10);
  }

}

void main()
{
   FILE*hs;
   typedef struct namnam{
   char nn[20];
   int at;
   long int sc;
   };
   namnam*name=new namnam[5];
   chance=3;
   score=10;

   for(int sy=0;sy<5;sy++)
	{
	name[sy].nn[0]='N';
	name[sy].nn[1]='O';
	name[sy].nn[2]='N';
	name[sy].nn[3]='E';
	name[sy].at=9;
	name[sy].sc=0;
	}
   int gd=DETECT, gm, errorcode;

   initgraph(&gd, &gm,"c:\\tc\\bgi");
   errorcode = graphresult();
   if (errorcode != grOk)
   {
      cout<<"Graphics error: "<< grapherrormsg(errorcode)<<"\n";;
      cout<<"Press any key to halt:";
      getch();
      exit(1);
   }
  // cout<<"\t"<<sizeof(name[sy]);
  // getch();
   if((hs=fopen("score.mbd","rb"))==NULL)
   {fclose(hs);}
   else
   {
   while(!feof(hs))
     {
     for(sy=0;sy<5;sy++)
	{
	fread(&name[sy],sizeof(name[sy]),1,hs);
	}
     break;
     }
     fclose(hs);
   }
   setfillstyle(1,14);
   bar(0,0,639,479);
   setcolor(2);
   settextstyle(4,0,9);
   setcolor(4);
   outtextxy(100,150,"PINBALL");
   getch();
   cleardevice();
   settextstyle(4,0,4);
   outtextxy(140,140,"How to play :");
   settextstyle(7,0,1);
   setcolor(14);
   outtextxy(200,200,"Use Right and left mouse");
   outtextxy(200,225,"clicks to move right and");
   outtextxy(200,250,"left flaps respectively.");
   getch();
   iota:
   //setbkcolor(n);

   cleardevice();

   setcolor(2);
   setfillstyle(1,n);
   bar(169,9,447,468);
   setcolor(2);

   line(170,16,170,460);
   line(176,10,440,10);
   line(176,466,440,466);
   line(446,16,446,460);
   line(170,16,176,10);
   line(176,466,170,460);
   line(440,466,446,460);
   line(446,16,440,10);


   int d[]={426,361,441,361,441,409,426,409,426,361};

     gotoxy(5,15);
     cout<<"SCORE : ";
     gotoxy(10,16);
     cout<<"        ";
     gotoxy(10,16);
     cout<<score;
     gotoxy(5,5);
     cout<<"HIGH SCORE : ";
     gotoxy(10,6);
     cout<<"        ";
     gotoxy(10,6);
     cout<<name[0].sc;

   setfillstyle(1,1);
   fillpoly(4,d);
   setlinestyle(0,1,3);
   setcolor(3);
   setfillstyle(1,6);
   line(174,14,174,462);
   line(174,14,442,14);
   line(442,14,442,462);
   line(442,462,174,462);
   arc(308,148,0,120,134);
   arc(308,147,149,207,134);
   arc(209,48,28,240,35);
   arc(308,155,2,60,118);
   //arc(308,153,2,53,100);
   setfillstyle(1,3);
   setcolor(10);
   circle(206,45,10);
   floodfill(206,45,10);
   setcolor(11);
   setfillstyle(1,6);
   circle(265,105,10);
   floodfill(265,105,11);
   circle(330,110,10);
   floodfill(330,110,11);
   circle(295,145,10);
   floodfill(295,145,11);
   setcolor(3);
   line(425,152,425,460);//start line
   line(425,360,442,360);
   line(425,410,442,410);
   floodfill(430,415,3);
   line(188,208,188,280);
   line(188,280,174,310);
   line(425,390,442,390);
   line(425,290,400,265);
   line(424,291,399,266);
   line(400,265,425,265);
   floodfill(415,270,3);
   line(425,195,400,220);
   line(424,194,399,219);
   line(400,220,425,220);
   floodfill(415,210,3);

   setcolor(1);
   line(425,225,425,260);
   setcolor(4);
   setlinestyle(2,1,3);
   line(425,225,425,260);//hole
   setcolor(3);
   setlinestyle(0,1,3);

   setfillstyle(2,4);
   floodfill(430,405,3);
   setfillstyle(1,5);
   int a[]={212,318,212,358,245,378,212,318},b[]={390,318,390,358,354,378,390,318};
   fillpoly(3,a);
   fillpoly(3,b);

   line(213,357,246,377);   //PADDING
   line(389,357,353,377);
   line(245,377,213,317);
   line(354,377,389,317);

   line(192,326,192,385);
   line(409,326,409,385);
   line(192,368,252,401);
   line(409,369,347,401);
   line(192,385,252,403);
   line(409,385,347,403);
   setfillstyle(1,8);
   setcolor(8);
   //FLAPS
     fillpoly(4,fl1x[0]);
     fillpoly(4,fl2x[0]);
   setcolor(3);
   setfillstyle(1,5);

   floodfill(400,380,3);
   floodfill(196,380,3);
   floodfill(176,16,3);
   floodfill(230,16,3);
   floodfill(440,20,3);
   floodfill(176,70,3);

   floodfill(180,210,3);
   setcolor(4);
   setlinestyle(SOLID_LINE,1,0);
   setfillstyle(1,4);
   fillellipse(433,353,4,4);

   if(chance==3)
   dis("Chances left","Three");
   else if(chance==2)
   dis("Chances left","Two");
   else if(chance==1)
   dis("Chances left","One");

   delay(1000);
   dis("Press a key","to Play.");
 //  for(int ty=50;ty<100;ty++)
 //  {
//	sound(7);
 //	delay(5000);
   //	nosound();
  // }
   getch();
   chance--;
   randomize();
   randomize();
   randomize();
   on=n;
   n=cols[random(5)];

   setfillstyle(2,4);
   fillpoly(4,d);
   setcolor(4);
   dis("Press a key","to Quit.");
   game();
   game1();
   if(chance>0)
   goto iota;
   int j=0;
   if((name[4].sc<score))
   {
   j=1;
   dis("You made a","High score");
   getch();
   int sy=4;
   while(sy>=0)
	{
	   sy--;
	   if((name[sy].sc<score)&&(sy>=0))
	   {
	      name[sy+1]=name[sy];
	   }
	   else
	   {
	      dis("Enter your","name:");
	      gotoxy(69,18);
	      gets(name[sy+1].nn);
	      name[sy+1].sc=score;
	      break;
	   }
	}
   hs=fopen("score.mbd","wb");
     for(sy=0;sy<5;sy++)
	{
	fwrite(&name[sy],sizeof(name[sy]),1,hs);
	}
     fclose(hs);
   }
   else
   {dis("You Loose","Bye!Bye!");
   getch();
   }
   cleardevice();
   if(j==1)
   {
   setbkcolor(0);
   int c=1;
   while(!kbhit())
   {
   setcolor(c);
   settextstyle(4,0,8);
   outtextxy(130,160,"High Score!!");
   sound(c);
   if(c==15)
   c=0;
   else
   c++;
   }
   nosound();
   }
   else
   {
   setcolor(10);
   settextstyle(4,0,5);
   outtextxy(150,200,"Astalavista Baby!!");
   }
   getch();
   cleardevice();
   hs=fopen("score.mbd","rb");
   while(!feof(hs))
     {
     for(int sy=0;sy<5;sy++)
	{
	fread(&name[sy],sizeof(name[sy]),1,hs);
	}
	break;
     }
     fclose(hs);
     hs=fopen("scsc.mdb","w");
     for(sy=0;sy<5;sy++)
	{
	fprintf(hs,"%ld\n",name[sy].sc);
	}
     fclose(hs);
     char scsc[5][10];
     hs=fopen("scsc.mdb","r");
     while(!feof(hs))
     {
     for(int sy=0;sy<5;sy++)
	{
	fscanf(hs,"%s\n",scsc[sy]);
	}
	break;
     }
     fclose(hs);
   setbkcolor(0);
   int c=1;
   while(!kbhit())
   {
   setcolor(c);
   settextstyle(4,0,6);
   outtextxy(164,100,"**High Score**");
   settextstyle(4,0,3);
   for(sy=0;sy<5;sy++)
	{
	outtextxy(190,200+(sy*30),name[sy].nn);
	outtextxy((190+(strlen(name[sy].nn))*22),200+(sy*30),": ");
	outtextxy((200+(strlen(name[sy].nn))*23),200+(sy*30),scsc[sy]);
	}
   sound(c);
   if(c==15)
	 c=0;
   else
	 c++;
   }
   closegraph();
   restorecrtmode();
}
