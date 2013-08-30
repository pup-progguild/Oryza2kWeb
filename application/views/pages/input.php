<style type="text/css">
    #editor {
        position: relative !important;
        border: 1px solid lightgray;
        margin: auto;
        height: 300px;
        width: 70%;
    }
</style>

<div class="page-header">
	<h1>ORYZA 2K Potential Yield Information</h1>
</div>

<div class="tabbable">
    <ul class="nav nav-tabs">
        <li class="active"><a href="#param" data-toggle="tab">Parameters</a></li>
        <li><a href="#advanced" data-toggle="tab">Advanced Input</a></li>
        <li><a href="#run" data-toggle="tab" id="tab-run">Run</a></li>
    </ul>
    <div class="tab-content">
        <div id="param" class="tab-pane active">
            <form name="main" id="main" method="get" class="form-horizontal">
                <div class="control-group" id="site-field">
                    <input type="hidden" id="site" name="site" value="PHL">
                    <label class="control-label" for="site-control">Site</label>
                    <div class="controls">
                        <div id="site-control" class="btn-group">
                            <a class="btn dropdown-toggle" data-toggle="dropdown" href="#">
                                <span id="site-value">PHL</span> <span class="caret"></span>
                            </a>
                            <ul class="dropdown-menu">
                                <?php for($i = 0; $i < count($sites); $i++): $site = $sites[$i] ?>
                                    <li<?= $i == 0 ? ' class="active"' : '' ?>><a href="#!site=<?= $site['country'] ?>"><?= $site['country'] ?></a></li>
                                <?php endfor ?>
                            </ul>
                        </div>
                    </div>
                </div>

                <div class="control-group" id="year-field">
                    <input type="hidden" id="year" name="year" value="1991">
                    <label class="control-label" for="year-control">Year</label>
                    <div class="controls">
                        <div id="year-control" class="btn-group">
                            <a class="btn dropdown-toggle" data-toggle="dropdown" href="#">
                                <span id="year-value">1991</span> <span class="caret"></span>
                            </a>
                            <ul class="dropdown-menu">
                                <? foreach($years as $year): ?>
                                    <li><a href="#!year=<?= $year['year'] ?>"><?= $year['year'] ?></a></li>
                                <? endforeach ?>
                            </ul>
                        </div>
                    </div>
                </div>

                <div class="control-group" id="variety-field">
                    <input type="hidden" id="variety" name="variety" value="s">
                    <label class="control-label" for="variety-control">Variety</label>
                    <div class="controls">
                        <div id="variety-control" class="btn-group">
                            <a class="btn dropdown-toggle" data-toggle="dropdown" href="#">
                                <span id="variety-value">Short-term duration</span> <span class="caret"></span>
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
                    <input type="hidden" id="sowing" name="sowing" value="d">
                    <label class="control-label" for="sowing-date-control">Date of sowing</label>
                    <div class="controls">
                        <div id="sowing-date-control" class="btn-group">
                            <a class="btn dropdown-toggle" data-toggle="dropdown" href="#">
                                <span id="sowing-value">Dec. 15 &ndash; Jan. 15</span> <span class="caret"></span>
                            </a>
                            <ul class="dropdown-menu">
                                <li><a href="#!sowing=d">Dec. 15 &ndash; Jan. 15</a></li>
                                <li><a href="#!sowing=w">Jun. 15 &ndash; Jul. 15</a></li>
                            </ul>
                        </div>
                    </div>
                </div>

                <div class="control-group" id="seeding-field">
                    <input type="hidden" id="seeding" name="seeding" value="d">
                    <label class="control-label" for="seeding-control">Seeding</label>
                    <div class="controls">
                        <div id="seeding-control" class="btn-group">
                            <a class="btn dropdown-toggle" data-toggle="dropdown" href="#">
                                <span id="seeding-value">Direct Seeding</span> <span class="caret"></span>
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

                <div style="height: 32px"></div>
            </form>
        </div>
        <div id="advanced" class="tab-pane">
            <form class="form-horizontal">
                <input type="hidden" id="variety-edit" name="variety-edit" value="short-term_duration">
                <div id="variety-edit-control" class="btn-group">
                    <a class="btn dropdown-toggle" data-toggle="dropdown" href="#">
                        <span id="variety-edit-value">Short-term duration</span> <span class="caret"></span>
                    </a>
                    <ul class="dropdown-menu">
                        <?php foreach ($template as $variety): ?>
                            <li><a href="#!variety-edit=<?= underscore($variety['label']) ?>">
                                    <?= $variety['label'] ?>
                                </a>
                            </li>
                        <?php endforeach ?>
                    </ul>
                </div>
                <div class="row-fluid">
                    <div class="span3">
                        <ul class="nav nav-list">
                            <li class="nav-header">Control files</li>
                            <li class="active"><a href="#!">Control data</a></li>
                            <li><a href="#!">Rerun data</a></li>
                            <li class="nav-header">Data files</li>
                            <li><a href="#!">Crop data</a></li>
                            <li><a href="#!">Experimental data</a></li>
                        </ul>
                    </div>
                    <div class="span3" id="editor"><? //echo html_escape($template[0]['control_dat']) ?></div>
                </div>





                <div style="height: 96px"></div>
            </form>
        </div>
        <div id="run" class="tab-pane">
            OMG RESULTS HERE
        </div>
    </div>
</div>

<!--
<script src="//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"></script>
<script>window.jQuery || document.write('<script src="<?= base_url() ?>js/vendor/jquery-1.9.1.min.js"><\/script>')</script>
-->
<script src="<?= base_url() ?>js/vendor/jquery-1.9.1.min.js"></script>
<script src="<?= base_url() ?>js/vendor/jquery.ba-hashchange.min.js"></script>
<!-- <script src="<?= base_url() ?>js/plugins.js"></script> -->
<script src="<?= base_url() ?>js/vendor/require.js"></script>
<script src="<?= base_url() ?>js/vendor/ace-builds/src-noconflict/ace.js" type="text/javascript"></script>
<script src="<?= base_url() ?>js/vendor/ace-builds/src-noconflict/mode-oryza_dat.js"></script>

<script>
    var buffer = [];
    var editor;

    (function() {
        var lastItem;

        function isTransplanted() {
            return $("#seeding").val() == "t";
        }

        function doLogic() {
            displayTransplField(isTransplanted());
        }

        function displayTransplField(b) {
            var transplField = $("#transpl-field");

            transplField.clearQueue();
            if(b)
                transplField.slideDown(250);
            else
                transplField.slideUp(250);
        }

        function validate() {
            var valid = true;

            valid &= $("#site").val() != "";
            valid &= $("#year").val() != "";
            valid &= $("#variety").val() != "";
            valid &= $("#sowing").val() != "";
            valid &= $("#seeding").val() != "";

            if(isTransplanted())
                valid &= $("#transpl").val() != "";

            return valid;
        }

        function readyEditor() {
            editor = ace.edit("editor");
            editor.setTheme("ace/theme/monokai");
            editor.getSession().setMode("ace/mode/oryza_dat");
        }

        function run() {
            if(!validate()) {
                alert("Invalid input");
            }
            else
                $("#main").submit();
        }

        $(window).hashchange(function() {
            var rawHash = location.hash.substr(1);
            var isHashEvent = rawHash.indexOf('!') == 0; // event data k/v pair(s) are specified after bang character.

            if(isHashEvent) {
                rawHash = rawHash.substr(1);
                var segments = rawHash.split('&');
                for(var i in segments) {
                    var segment = segments[i];
                    var pairDivider = segment.indexOf('=');
                    var isSegmentPair = pairDivider != -1;

                    var key = segment.substr(0, isSegmentPair ? pairDivider : segment.length);
                    var value = isSegmentPair ? segment.substr(pairDivider + 1) : true;

                    $("#" + key).val(value);
                    $("#" + key + "-value").html(lastItem);

                    doLogic();
                }
            }
        });

        $(".dropdown-menu").find("li").find("a").click(function() {
            $(this).parent().parent().find("li").removeClass("active");
            $(this).parent().addClass("active");
            lastItem = $(this).text();
        });

        $("#tab-run").click(function() {
            run();
        });

        $(document).ready(function() {
            $("#transpl-field").hide();
            location.hash = "";
            readyEditor();

            $("#editor").val(buffer);
        })
    })();
</script>
