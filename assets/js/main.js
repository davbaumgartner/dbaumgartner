var resizeImages = function () {
	var imgs = document.getElementsByClassName("pimage"), pNode = null;
	for(var i = 0, ci = imgs[i]; i < imgs.length; i += 1) {
		pNode = (imgs[i]).parentNode;
		(imgs[i]).style.height = (window.innerHeight- 120) + "px";
		pNode.style.backgroundColor = "#000"
	};
}
window.addEventListener('load',resizeImages,false);
window.addEventListener('resize',resizeImages,false);