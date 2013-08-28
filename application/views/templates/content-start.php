<!--[if lt IE 7]>
<p class="chromeframe">You are using an <strong>outdated</strong> browser. Please <a href="http://browsehappy.com/">upgrade
    your browser</a> or <a href="http://www.google.com/chromeframe/?redirect=true">activate Google Chrome Frame</a> to
    improve your experience.</p>
<![endif]-->

<!-- Add your site or application content here -->
<!-- Part 1: Wrap all page content here -->
<div id="wrap">
    <!-- Fixed navbar -->
    <div class="navbar navbar-fixed-top">
        <div class="navbar-inner">
            <div class="container">
                <button type="button" class="btn btn-navbar" data-toggle="collapse" data-target=".nav-collapse">
                    <span class="icon-bar"></span>
                    <span class="icon-bar"></span>
                    <span class="icon-bar"></span>
                </button>
                <a class="brand" href="<?= base_url() ?>">ORYZA 2K Web</a>

                <div class="nav-collapse collapse">
                    <ul class="nav">
                        <li <? if($title == "Home"): ?>class="active"<? endif ?>><a href="<?= base_url() ?>index.php/oryza">Home</a></li>
                        <li <? if($title == "Input"): ?>class="active"<? endif ?>><a href="<?= base_url() ?>index.php/input">Simulation</a></li>
                        <li <? if($title == "About"): ?>class="active"<? endif ?>><a href="<?= base_url() ?>index.php/about">About</a></li>
                    </ul>
                </div>
                <!--/.nav-collapse -->
            </div>
        </div>
    </div>

    <!-- Begin page content -->
    <div class="container">