<?php require('pages.php') ?>
			<!-- Fixed navbar -->
			<div class="navbar navbar-fixed-top">
				<div class="navbar-inner">
					<div class="container">
						<button type="button" class="btn btn-navbar" data-toggle="collapse" data-target=".nav-collapse">
							<span class="icon-bar"></span>
							<span class="icon-bar"></span>
							<span class="icon-bar"></span>
						</button>
						<a class="brand" href="index.php">ORYZA 2K Web</a>
						<div class="nav-collapse collapse">
							<ul class="nav">
								<li <?php if($_TITLE['.'] == $_TITLE['/index.php']) : ?>class="active"<?php endif ?>><a href="index.php"><?php echo $_TITLE['/index.php'] ?></a></li>
								<li class="dropdown">
									<a href="#" class="dropdown-toggle" data-toggle="dropdown">Simulation <b class="caret"></b></a>
									<ul class="dropdown-menu">
										<li><a href="input.php"><?php echo $_TITLE['/input.php'] ?></a></li>
										<li><a href="#"><?php echo $_TITLE['/files.php'] ?></a></li>
									</ul>
								</li>

								<li <?php if($_TITLE['.'] == $_TITLE['/about.php']) : ?>class="active"<?php endif ?>><a href="about.php"><?php echo $_TITLE['/about.php'] ?></a></li>
							</ul>
						</div><!--/.nav-collapse -->
					</div>
				</div>
			</div>