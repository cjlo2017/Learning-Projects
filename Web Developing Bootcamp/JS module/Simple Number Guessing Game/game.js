alert("hello");

// secret number
var magicNumber = 7;

// ask for guess
var guessString = prompt("Guess a number");
var guess =  Number(guessString);

if (guess === magicNumber){
	console.log("Correct");
} else if (guess > magicNumber) {
	console.log("too high");
} else {
	console.log("too low");
}