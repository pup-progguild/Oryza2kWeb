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
    }

    public function index() {
        $data['title'] = 'Simulation';

        $data['template'] = $this->run_templates_data_model->get_template();
        $data['weather_years'] = $this->weather_data_model->get_country_year_list();
        $data['years'] = $this->weather_data_model -> get_years();
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
        write_file('./temp/'.$template_data['file_prefix'].'.exp', $experiment_data_dat);
        write_file('./temp/'.$template_data['file_prefix'].'.crp', $crop_data_dat);

        foreach ($weather_data as $weather) {
            write_file('./temp/'. $weather['country_code'] . $weather['station_code'] .'.'. substr($weather['year'],1,3), $weather['data']);
        }

        exec('./home/nix/www/oryza2kweb/temp/oryza2000 control.dat', $exec_output = array());

        header("Content-Type: text/plain");

        echo $control_dat;

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
        /*
        if ($year > $first_year['year']) {
            $count = 1;
            for($i = $first_year['year'] + 1; $i <= $year; $i++) {
                $rerun_dat = $rerun_dat . "* rerun # {$count}\r\nIYEAR = {$i}\r\nEMYR = {$i} \r\n";
                $count++;
            }
        } elseif ($year === $first_year['year']) {
            $experiment_data_dat = preg_replace("/(IYEAR)(\\s*)(=)(\\s+)((?:(?:[1]{1}\\d{1}\\d{1}\\d{1})|(?:[2]{1}\\d{3})))(?![\\d])/", 'IYEAR = '. $year, $experiment_data_dat);
            $experiment_data_dat = preg_replace("/(EMYR)(\\s*)(=)(\\s+)((?:(?:[1]{1}\\d{1}\\d{1}\\d{1})|(?:[2]{1}\\d{3})))(?![\\d])/", 'EMYR = '. $year, $experiment_data_dat);
        } else {
            echo 'wrong' . $year . $first_year['year'];
        }
        */




        $station_code = $this->weather_data_model->get_station_code($site);

        $experiment_data_dat = preg_replace("/(CNTR)(\\s*)(=)(\\s*)(\\'.*?\\')/", 'CNTR = \'' . $station_code['country_code'] .'\'', $experiment_data_dat);
        $experiment_data_dat = preg_replace("/(ISTN)(\\s*)(=)(\\s*)(\\d+)/", 'ISTN = ' . $station_code['station_code'], $experiment_data_dat);

        $experiment_data_dat = preg_replace("/(STTIME)(\\s*)(=)(\\s*)(\\d+)/", 'STTIME = ' . $dateofsowing, $experiment_data_dat);

        if ($seeding === 't') {
            $experiment_data_dat = preg_replace("/(ESTAB)\\s*(=)\\s*(\\'.*?\\')/", 'ESTAB = \'TRANSPLANT\'', $experiment_data_dat);
            $experiment_data_dat = preg_replace("/(SBDUR)(\\s*)(=)(\\s*)(\\d+)/", 'SDBUR = ' . $sdbdur, $experiment_data_dat);
        }
        elseif ($seeding === 'd')
            $experiment_data_dat = preg_replace("/(ESTAB)\\s*=\\s*(\\'.*?\\')/", 'ESTAB = \'DIRECT-SEED\'', $experiment_data_dat);
        else
            show_404();

        return $experiment_data_dat;
    }
}