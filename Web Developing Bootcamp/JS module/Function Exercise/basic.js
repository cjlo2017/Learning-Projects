function isEven(num){
	if(num%2===0) return true;
	return false;
}

function factorial(n){
	var acc = 1;
	for(var i=n;i>0;i--){
		acc *= i;
	}
	return acc;
}
function kebabToSnake(string){
	return string.replace(/-/g,'_');
}