<?php include('header.php') ?>
<?php include('content-start.php') ?>
<div class="page-header">
	<h1>ORYZA 2K Potential Yield Information</h1>
</div>
<form name="main" id="main" method="get" class="form-horizontal">
	<div class="control-group">
		<label class="control-label" for="site">Site</label>
		<div class="controls">
			<input id="site" name="site" type="text" value="IRRI" disabled>
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
						<li><a href="#"><?php echo $y ?></a></li>
					<?php endfor; ?>
				</ul>
			</div>
		</div>
	</div>

	<div class="control-group">
		<input type="hidden" id="variety" name="variety" value="short">
		<label class="control-label" for="variety-control">Variety</label>
		<div class="controls">
			<div id="variety-control" class="btn-group">
				<a class="btn dropdown-toggle" data-toggle="dropdown" href="#">
					<span id="variety-value"></span> <span class="caret"></span>
				</a>
				<ul class="dropdown-menu">
					<li><a href="#">Short-term duration</a></li>
					<li><a href="#">Medium-term duration</a></li>
					<li><a href="#">Long-term duration</a></li>
				</ul>
			</div>
		</div>
	</div>

	<div class="control-group">
		<input type="hidden" id="sowing" name="sowing" value="dry">
		<label class="control-label" for="sowing-date-control">Date of sowing</label>
		<div class="controls">
			<div id="sowing-date-control" class="btn-group">
				<a class="btn dropdown-toggle" data-toggle="dropdown" href="#">
					<span id="sowing-date-value"></span> <span class="caret"></span>
				</a>
				<ul class="dropdown-menu">
					<li><a href="#">Dec. 15 &ndash; Jan. 15</a></li>
					<li><a href="#">Jun. 15 &ndash; Jul. 15</a></li>
				</ul>
			</div>
		</div>
	</div>

	<div class="control-group">
		<input type="hidden" id="seeding" name="seeding" value="direct">
		<label class="control-label" for="seeding-control">Seeding</label>
		<div class="controls">
			<div id="seeding-control" class="btn-group">
				<a class="btn dropdown-toggle" data-toggle="dropdown" href="#">
					<span id="seeding-value"></span> <span class="caret"></span>
				</a>
				<ul class="dropdown-menu">
					<li class="active"><a href="#">Direct Seeding</a></li>
					<li><a href="#">Transplanted</a></li>
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
<?php include('content-end.php') ?>
<script>
	// TODO hook events
	var itemSeeding = $("#seeding-control").find("ul").find("li");

	$(itemSeeding).click(function(evt) {
		target = $(evt.target);
		$("#seeding").val(target.attr("data-value"));
	})

	$("#seeding").change(function() {
		alert('a');
	});
</script>
<?php include('footer.php') ?>
