<?php
/**
 * Super Class
 *
 * @package	    Input
 * @subpackage	Subpackage
 * @category	Category
 * @author	    hoshi~
 * @link	    https://github.com/awkwardusername
 * @date        8/28/13 | 3:22 AM
 */

class Input extends CI_Controller {
    public function __construct() {
        parent::__construct();

        $this->load->helper(array('url', 'inflector'));

        $this->load->model('run_templates_data_model');
        $this->load->model('weather_data_model');
        $this->load->model('run_cache_model');
    }

    public function index() {
        $data['title'] = 'Simulation';

        $data['template'] = $this->run_templates_data_model->get_template();
        $data['weather_years'] = $this->weather_data_model->get_country_year_list();
        $data['years'] = $this->weather_data_model -> get_years();
        $data['first_year'] = $this->weather_data_model -> get_first_year();
        $data['sites'] = $this->weather_data_model -> get_countries();

        $this->load->view('templates/header', $data);
        $this->load->view('templates/content-start', $data);
        $this->load->view('pages/input', $data);
        $this->load->view('templates/content-end', $data);
        $this->load->view('templates/footer', $data);
    }

    public function retrieve_template($variety, $file) {
        $template = $this->run_templates_data_model -> get_template();
        header("Content-Type: text/plain");
        echo $template[$variety][$file];
    }

    public function save_template() {
        $this -> run_templates_data_model -> add();
    }

    /*
     * @function         simulate_basic
     * @description      runs a simulation using basic input
     * @params           $site          sets the location of weather data, maps to CNTR and ISTN at *.exp file
     *                   $year          sets the year of weather data, maps to IYEAR = EMYR at *.exp file
     *                   $variety       fetches the template for getting the *.exp and *.crp data
     *                   $dateofsowing  sets the day of year of the start day of simulation, maps to STTIME at *.exp
     *                   $seeding       sets method of seeding, maps to ESTAB at *.exp
     *                   $sdbdur        sets seedbed duration on ESTAB = 'TRANSPLANT'.
     *
     * eg. <?= base_url ?>input/simulate_basic/phil/1991/long_term/0?/d/45
     */
    public function simulate_basic($site, $year, $variety, $dateofsowing, $seeding, $sdbdur) {
        $template_data = $this->run_templates_data_model->get_template($variety);
        $weather_data = $this->weather_data_model->get_weather_from_country_till_selected_years($site,$year);

        $control_dat = $template_data['control_dat'];
        $experiment_data_dat = $template_data['experiment_data_dat'];
        $crop_data_dat = $template_data['crop_data_dat'];

        // echo $control_dat;

        $control_dat = $this->modify_control_dat($control_dat, $template_data['file_prefix']);
        $experiment_data_dat = $this->modify_experiment_data_dat($experiment_data_dat, $site, $year, $dateofsowing, $seeding, $sdbdur);

        write_file('./temp/control.dat', $control_dat);
        write_file('./temp/'.$template_data['file_prefix'].'.crp', $crop_data_dat);
        write_file('./temp/reruns.dat', $experiment_data_dat['reruns']);
        write_file('./temp/'.$template_data['file_prefix'].'.exp', $experiment_data_dat['experiment_data']);

        foreach ($weather_data as $weather) {
            write_file('./temp/'. $weather['country_code'] . $weather['station_code'] .'.'. substr($weather['year'],1,3), $weather['data']);
        }

        exec('./temp/oryza2000 control.dat', $exec_output = array());

        header("Content-Type: text/plain");

        echo sha1($site.$year.$variety.$dateofsowing.$seeding.$sdbdur);

        //print_r($exec_output);
    }

    private function modify_control_dat($control_dat, $file_prefix) {
        $control_dat = preg_replace("/(FILEIT)(\\s*)(=)(\\s*)(\\'.*?\\')/", 'FILEIT = \''.$file_prefix.'.exp\'', $control_dat, 1);
        $control_dat = preg_replace("/(FILEI1)(\\s*)(=)(\\s*)(\\'.*?\\')/", 'FILEI1 = \''.$file_prefix.'.crp\'', $control_dat, 1);

        return $control_dat;
    }

    private function modify_experiment_data_dat($experiment_data_dat, $site, $year, $dateofsowing, $seeding, $sdbdur) {
        $first_year = $this->weather_data_model->get_first_year();
        $rerun_dat = '';

        if ($year >= $first_year['year']) {
            if ($dateofsowing > 0) {
                $count = 1;
                for ($i = $first_year['year']; $i <= $year; $i++) {
                    $day = date('L', strtotime("$i-1-1")) ? 366 : 365;
                    for ($j = 1; $j <= $day; $j++) {
                        $rerun_dat = $rerun_dat . "* rerun # {$count}\r\nIYEAR = {$i}\r\nEMYR = {$i} \r\n";
                        $rerun_dat = $rerun_dat . "EMD = {$j}\r\n";
                    }
                    $count++;
                }
            }
        } elseif ($year === $first_year['year']) {
            $experiment_data_dat = preg_replace("/(IYEAR)(\\s*)(=)(\\s+)((?:(?:[1]{1}\\d{1}\\d{1}\\d{1})|(?:[2]{1}\\d{3})))(?![\\d])/", 'IYEAR = '. $year, $experiment_data_dat);
            $experiment_data_dat = preg_replace("/(EMYR)(\\s*)(=)(\\s+)((?:(?:[1]{1}\\d{1}\\d{1}\\d{1})|(?:[2]{1}\\d{3})))(?![\\d])/", 'EMYR = '. $year, $experiment_data_dat);
        } else {
            echo 'wrong' . $year . $first_year['year'];
        }
        $station_code = $this->weather_data_model->get_station_code($site);

        $experiment_data_dat = preg_replace("/(CNTR)(\\s*)(=)(\\s*)(\\'.*?\\')/", 'CNTR = \'' . $station_code['country_code'] .'\'', $experiment_data_dat);
        $experiment_data_dat = preg_replace("/(ISTN)(\\s*)(=)(\\s*)(\\d+)/", 'ISTN = ' . $station_code['station_code'], $experiment_data_dat);

        if($dateofsowing !== 0)
            $experiment_data_dat = preg_replace("/(STTIME)(\\s*)(=)(\\s*)(\\d+)/", 'STTIME = ' . $dateofsowing, $experiment_data_dat);

        if ($seeding === 't') {
            $experiment_data_dat = preg_replace("/(ESTAB)\\s*(=)\\s*(\\'.*?\\')/", 'ESTAB = \'TRANSPLANT\'', $experiment_data_dat);
            $experiment_data_dat = preg_replace("/(SBDUR)(\\s*)(=)(\\s*)(\\d+)/", 'SDBUR = ' . $sdbdur, $experiment_data_dat);
        }
        elseif ($seeding === 'd')
            $experiment_data_dat = preg_replace("/(ESTAB)\\s*=\\s*(\\'.*?\\')/", 'ESTAB = \'DIRECT-SEED\'', $experiment_data_dat);
        else
            show_404();

        return array('reruns' => $rerun_dat, 'experiment_data' => $experiment_data_dat);
    }

    public function get_all_time_average_temp($country_code, $from, $to) {
        $year_avg_temp = array();
        $years = $this->weather_data_model->get_years_by_country($country_code);
        foreach($years as $year)
            $year_avg_temp[$year] = get_average_temp_for_year($country_code, $year);

        echo json_encode($year_avg_temp);
    }

    public function get_average_temp_for_year($country_code, $year) {
        $avg_temp = array();
        $data = parse_weather_data($country_code, $year, "php_array", FALSE);
        foreach($data as $day => $day_data)
            $avg_temp[] = $day_data[$day][6]; // avg min/max temp

        $real_avg_temp = 0;
        foreach($avg_temp as $individual_avg_temp)
            $real_avg_temp += $avg;
        return ($real_avg_temp /= count($individual_avg_temp));
    }

    public function parse_weather_data($country_code, $year, $output = "json", $echo = TRUE) {
        $weather_data = $this->weather_data_model->get_weather_data($country_code,$year);

        $re1='(\\s+)(\\d+)(\\s+)((?:(?:[1]{1}\\d{1}\\d{1}\\d{1})|(?:[2]{1}\\d{3})))(?![\\d])(\\s+)(\\d+)(\\s+)(\\d+)(\\s+)([+-]?\\d*\\.\\d+)(?![-+0-9\\.])(\\s+)([+-]?\\d*\\.\\d+)(?![-+0-9\\.])(\\s+)([+-]?\\d*\\.\\d+)(?![-+0-9\\.])(\\s+)([+-]?\\d*\\.\\d+)(?![-+0-9\\.])(\\s+)([+-]?\\d*\\.\\d+)(?![-+0-9\\.])';	# Float 5

        $weather_data_array = explode("\n",$weather_data['data']);

        $s = 0;

        foreach ($weather_data_array as $data) {
            if (preg_match_all ("/$re1/is", $data, $matches)) {

                $int1=$matches[2][0];
                $year1=$matches[4][0];
                $int2=$matches[6][0];
                $int3=$matches[8][0];
                $float1=$matches[10][0];
                $float2=$matches[12][0];
                $avg = ((float)$float1 + (float)$float2) / 2; // avg temperature
                $float3=$matches[14][0];
                $float4=$matches[16][0];
                $float5=$matches[18][0];

                $graph_me[$s++] = array($int1,$year1,$int2,$int3,$float1,$float2,$avg,$float3,$float4,$float5);
            }
        }

        switch(strtolower($output)) {
            case "json":
                $graph_me = json_encode($graph_me);
                break;
            case "php_array":
                // by default it returns a PHP array
                break;
        }

        if($echo) {
            echo $graph_me;
            return FALSE;
        }
        else {
            return $graph_me;
        }
    }
}