import java.util.Collections;
import ddf.minim.*;

Minim minim;
AudioSample player;

int numberOfItems;
int numberOfTrials;
int pretestDelay;
int testDelay;
int posttestDelay;
int postinputDelay;

String inputString = new String();
String displayString = new String();
ArrayList testStrings = new ArrayList();
String previousStrings[];
String enteredStrings = new String();
String outputFile;
String legalChars;

final int INTRO=0;
final int PRETEST=1;
final int TEST=2;
final int POSTTEST=3;
final int INPUT=4;
final int FINALINTRO=6;
final int FINALINPUT=7;
final int INPUTERROR=8;
final int OUTRO=9;

int progState;
int lastState;
int stringCounter=0;
int trialCounter=0;
long now;
int offset;

String[] finalStrings;

PFont fontText;
PFont fontMono;

void setup() 
{ 
  size(800, 600); 
  smooth();
  fontText = loadFont("HelveticaNeue-25.vlw");
  fontMono = loadFont("Monospaced-48.vlw"); 
  textAlign(CENTER); 
  previousStrings=loadStrings(selectInput());
  String[] parameters=split(previousStrings[0],',');

  numberOfItems=Integer.parseInt(parameters[0]);
  numberOfTrials=Integer.parseInt(parameters[1]);
  pretestDelay=Integer.parseInt(parameters[2]);
  testDelay=Integer.parseInt(parameters[3]);
  posttestDelay=Integer.parseInt(parameters[4]);
  legalChars=parameters[5];

  finalStrings = new String[numberOfItems];
  offset=previousStrings.length-numberOfItems;
  for (int i=0; i<numberOfItems; i++) {
    testStrings.add(previousStrings[i+offset]);
  }
  java.util.Collections.shuffle(testStrings);
  progState=INTRO;
  lastState=-1;
  outputFile=selectOutput();
  minim=new Minim(this);
  player=minim.loadSample("beep.mp3");
} 

void draw() 
{
  background(255,255,255);
  textFont(fontMono);
  textAlign(CENTER);
  fill(0);
  switch(progState) {
  case INTRO:
    textFont(fontText);
    fill(0,0,255);
    text("Thank you for agreeing to participate in this study.\n\n" +
      "During the experiment you will see a series of letter strings appear on the screen. " +
      "We would like to see how well you can " +
      "learn them. After each string appears, there will be a short delay before you are " +
      "allowed to type in what you think you saw. Try to remember the strings as accurately " +
      "as possible. You can use the backspace button if you make a mistake, and can press " +
      "ENTER to see the next string.\n\n" +
      "Please press the track-pad button when you are ready to begin, and good luck.", 
    20,100,width-20,height);
    break;

  case PRETEST:
    text("",width/2,height/2);
    stateChange(pretestDelay,TEST);
    break;
  case TEST:
    text((String)testStrings.get(stringCounter),width/2,height/2);
    stateChange(testDelay,POSTTEST);
    break;
  case POSTTEST:
    text("",width/2,height/2);
    stateChange(posttestDelay,INPUT);
    break;
  case INPUT:
    if ((millis()/300)%2==1) text(inputString+" ",width/2,height/2);
    else text(inputString+"_",width/2,height/2);
    break;
  case FINALINTRO:
    fill(0,0,255);
    textFont(fontText); 
    text("Thank you!\n\n" +
      "You just saw " + Integer.toString(numberOfItems) + " different strings. We would like you to " +
      "try to recall all of them now as best you can. Please keep going until you have tried to remember each one.\n" + 
      "We will give you an indication of how many you have left to enter.\n" + 
      "However, we won't tell you how many you got right until the end of the experiment.\n\n" + 
      "Press the track-pad button to begin.",10,100,width-10,height);
    break;
  case FINALINPUT:
    if ((millis()/300)%2==1) text(inputString+" ",width/2,height/2);
    else text(inputString+"_",width/2,height/2);
    textFont(fontText);
    textAlign(LEFT);
    text("Remaining: "+Integer.toString(numberOfItems-stringCounter),30,30);
    break;
  case INPUTERROR:
    fill(255,0,0);
    textFont(fontText);
    text("You have already entered this string.\nPlease try again." +
      "\n\nPress the track-pad button when you are ready.",10,100,width-10,height);
    break;
  case OUTRO:
    fill(0,0,255);
    textFont(fontText);
    text("Thank you for taking part\nin our experiment!\nYou got "+Integer.toString(score(testStrings,finalStrings))+" correct.",width/2,height*.25);
    break;
  }
}

void stateChange(long d,int s)
{
  if (progState!=lastState)
  {
    now=millis();
    lastState=progState;
  }
  else if (millis()>now+d) progState=s;
}  

void mousePressed() {
  if (progState==INTRO) progState=PRETEST;
  if (progState==FINALINTRO) progState=FINALINPUT;
  if (progState==INPUTERROR) progState=FINALINPUT;
  if (progState==OUTRO) {
    player.close();
    minim.stop();
    super.stop();
    exit();
  }
} 

void keyPressed() 
{ 
  if (progState==INPUT || progState==FINALINPUT) {
    switch (key) {
    case ENTER:
      if (progState==INPUT && inputString.length()>0) {
        enteredStrings=enteredStrings +
          (String)testStrings.get(stringCounter) + '\t' +
          inputString + '\n';
        inputString = ""; 
        stringCounter++;
        if (stringCounter==numberOfItems) {
          trialCounter++;
          stringCounter=0;
          java.util.Collections.shuffle(testStrings);
        }
        if (trialCounter==numberOfTrials) progState=FINALINTRO;
        else progState=PRETEST;
      }
      if (progState==FINALINPUT && inputString.length()>0) {
        if (inStrings(finalStrings,inputString,stringCounter)) {
          inputString = "";
          progState=INPUTERROR;
        }
        else {
          finalStrings[stringCounter]=inputString;
          enteredStrings=enteredStrings + inputString;
          inputString = ""; 
          stringCounter++;
          if (stringCounter==numberOfItems) {
            saveStrings(outputFile,split(previousStrings[0]+'\n'+enteredStrings,'\n'));
            progState=OUTRO;
          }
          enteredStrings=enteredStrings+'\n';
        }
      }
      break;
    case BACKSPACE:
      if(inputString.length() > 0) { 
        inputString = inputString.substring(0, inputString.length() - 1); 
      }
      break; 
    default:
      if (legalChars.indexOf(key)!=-1) inputString = inputString + key;
      else if (key>=' ' && key<='z') player.trigger();
    }
  }
  else player.trigger();
} 

boolean inStrings(String[] a, String s, int c) {
  for (int i=0; i<c; i++) {
    if (a[i].equals(s)) return true;
  }
  return false;
}

int score(ArrayList in, String[] out) {
  int score=0;
  for (int i=0; i<out.length; i++) {
    if (inStrings(out,(String)in.get(i),out.length)) score++;
  }
  return score;
}  
