<?php
$_TITLE = array(
	"/index.php" => "Home",
	"/input.php" => "Input",
	"/files.php" => "Edit Files&hellip;",
	"/about.php" => "About"
);

$_TITLE['.'] = $_TITLE[substr($_SERVER['PHP_SELF'], strrpos($_SERVER['PHP_SELF'], '/'))];
