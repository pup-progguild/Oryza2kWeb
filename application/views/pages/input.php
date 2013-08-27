<div class="page-header">
	<h1>ORYZA 2K Potential Yield Information</h1>
</div>
<form name="main" id="main" method="get" class="form-horizontal">
    <div class="control-group" id="site-field">
        <input type="hidden" id="site" name="year" value="0">
        <label class="control-label" for="site-control">Site</label>
        <div class="controls">
            <div id="site-control" class="btn-group">
                <a class="btn dropdown-toggle" data-toggle="dropdown" href="#">
                    <span id="site-value"></span> <span class="caret"></span>
                </a>
                <ul class="dropdown-menu">
                    <li><a href="#!site=ph">Philippines</a></li>
                    <li><a href="#!site=in">Indonesia</a></li>
                    <li><a href="#!site=ch">Chinese</a></li>
                </ul>
            </div>
        </div>
    </div>

	<div class="control-group" id="year-field">
		<input type="hidden" id="year" name="year" value="0">
		<label class="control-label" for="year-control">Year</label>
		<div class="controls">
			<div id="year-control" class="btn-group">
				<a class="btn dropdown-toggle" data-toggle="dropdown" href="#">
					<span id="year-value"></span> <span class="caret"></span>
				</a>
				<ul class="dropdown-menu">
					<?php for($y = 1991; $y <= 1993; $y++) : ?>
						<li><a href="#!year=<?= $y ?>"><?php echo $y ?></a></li>
					<?php endfor; ?>
				</ul>
			</div>
		</div>
	</div>

	<div class="control-group" id="variety-field">
		<input type="hidden" id="variety" name="variety" value="short">
		<label class="control-label" for="variety-control">Variety</label>
		<div class="controls">
			<div id="variety-control" class="btn-group">
				<a class="btn dropdown-toggle" data-toggle="dropdown" href="#">
					<span id="variety-value"></span> <span class="caret"></span>
				</a>
				<ul class="dropdown-menu">
					<li><a href="#!variety=s">Short-term duration</a></li>
					<li><a href="#!variety=m">Medium-term duration</a></li>
					<li><a href="#!variety=l">Long-term duration</a></li>
				</ul>
			</div>
		</div>
	</div>

	<div class="control-group" id="sowing-field">
		<input type="hidden" id="sowing" name="sowing" value="dry">
		<label class="control-label" for="sowing-date-control">Date of sowing</label>
		<div class="controls">
			<div id="sowing-date-control" class="btn-group">
				<a class="btn dropdown-toggle" data-toggle="dropdown" href="#">
					<span id="sowing-date-value"></span> <span class="caret"></span>
				</a>
				<ul class="dropdown-menu">
					<li><a href="#!sowing=d">Dec. 15 &ndash; Jan. 15</a></li>
					<li><a href="#!sowing=w">Jun. 15 &ndash; Jul. 15</a></li>
				</ul>
			</div>
		</div>
	</div>

	<div class="control-group" id="seeding-field">
		<input type="hidden" id="seeding" name="seeding" value="direct">
		<label class="control-label" for="seeding-control">Seeding</label>
		<div class="controls">
			<div id="seeding-control" class="btn-group">
				<a class="btn dropdown-toggle" data-toggle="dropdown" href="#">
					<span id="seeding-value"></span> <span class="caret"></span>
				</a>
				<ul class="dropdown-menu">
					<li><a href="#!seeding=d">Direct Seeding</a></li>
					<li><a href="#!seeding=t">Transplanted</a></li>
				</ul>
			</div>
		</div>
	</div>

	<div class="control-group" id="transpl-field">
		<label class="control-label" for="transpl">Transplanting day</label>
		<div class="controls">
			<input id="transpl" name="transpl" type="text">
		</div>
	</div>

	<div class="control-group">
		<div class="controls">
			<button type="submit" class="btn btn-primary" data-loading>Run</button>
		</div>
	</div>
</form>

<!--
<script src="//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"></script>
<script>window.jQuery || document.write('<script src="<?= base_url() ?>js/vendor/jquery-1.9.1.min.js"><\/script>')</script>
-->
<script src="<?= base_url() ?>js/vendor/jquery-1.9.1.min.js"></script>
<script src="<?= base_url() ?>js/vendor/jquery.ba-hashchange.min.js"></script>
<script src="<?= base_url() ?>js/plugins.js"></script>

<script>
    $(window).hashchange(function() {
        alert("A");
        /*
         var rawHash = location.hash;
         var isHashEvent = rawHash.indexOf('!') == 0;

         if(isHashEvent) {
         var events = rawHash.split('&');
         for(var i in events) {
         alert(i);
         }
         }
         */
    });

    $(".dropdown-menu").find("li").click(function() {
        //change
    });
</script>
