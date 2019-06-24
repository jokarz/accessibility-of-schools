document.title = 'BURP - Balancing Urban Residential Planning'

$( ".map-controls" ).draggable({ containment: "parent" });

$('.control-elements').on('change', function(){
    if($('.map-controls').outerHeight() > 200){
        $('.map-controls').css({'height': $('.control-elements').outerHeight() + 
        $('.panelbar').outerHeight() + 30})
    }
})

let settingsOpen = true;
let currheight = 0
$('.minimise').on('click', function(e){
    if (settingsOpen){
        currheight = $('.map-controls').outerHeight();
        $('.minimise').html('<i class="typcn typcn-arrow-maximise"></i>');
        $('.map-controls').css({'height': '50px'})
        $('.control-elements').css({'display': 'none'})
        settingsOpen = false;
    }else{
        $('.minimise').html('<i class="typcn typcn-arrow-minimise"></i>');
        $('.map-controls').css({'height': currheight+'px'})
        $('.control-elements').css({'display': 'block'})
        settingsOpen = true;
    }
})