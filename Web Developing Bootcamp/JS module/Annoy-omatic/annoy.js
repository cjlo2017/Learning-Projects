var read = prompt("Are we there yet?");
while(read.indexOf("yes")===-1  && read.indexOf("yeah")===-1){
	read = prompt("Are we there yet?");
}
alert("Yay, we finally made it!");