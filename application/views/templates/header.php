<!DOCTYPE html>
<!--[if lt IE 7]>      <html class="no-js lt-ie9 lt-ie8 lt-ie7"> <![endif]-->
<!--[if IE 7]>         <html class="no-js lt-ie9 lt-ie8"> <![endif]-->
<!--[if IE 8]>         <html class="no-js lt-ie9"> <![endif]-->
<!--[if gt IE 8]><!--> <html class="no-js"> <!--<![endif]-->
	<head>
		<meta charset="utf-8">
		<meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1">
		<title>ORYZA 2K Web<?= (isset($title) && !empty($title) && $title != "Home" ? " &mdash; $title" : "") ?></title>
		<meta name="description" content="">
		<meta name="viewport" content="width=device-width">

		<!-- Place favicon.ico and apple-touch-icon.png in the root directory -->

		<link rel="stylesheet" href="<?= base_url() ?>css/normalize.css">
		<link rel="stylesheet" href="<?= base_url() ?>css/main.css">

		<!-- Bootstrap -->
		<link rel="stylesheet" href="<?= base_url() ?>css/bootstrap/bootstrap.min.css">
		<link rel="stylesheet" href="<?= base_url() ?>css/bootstrap/bootstrap-responsive.min.css">


		<style type="text/css">
			html,
			body {
				height: 100%;
				/* The html and body elements cannot have any padding or margin. */
			}

				/* Wrapper for page content to push down footer */
			#wrap {
				min-height: 100%;
				height: auto !important;
				height: 100%;
				/* Negative indent footer by it's height */
				margin: 0 auto -60px;
			}

				/* Set the fixed height of the footer here */
			#push,
			#footer {
				height: 60px;
			}
			#footer {
				background-color: #f5f5f5;
			}

				/* Lastly, apply responsive CSS fixes as necessary */
			@media (max-width: 767px) {
				#footer {
					margin-left: -20px;
					margin-right: -20px;
					padding-left: 10px;
					padding-right: 10px;
				}
			}



				/* Custom page CSS
			  -------------------------------------------------- */
				/* Not required for template or sticky footer method. */

			#wrap > .container {
				padding-top: 60px;
			}
			.container .credit {
				margin: 20px 0;
			}

			code {
				font-size: 80%;
			}
		</style>

	</head>
	<body>
