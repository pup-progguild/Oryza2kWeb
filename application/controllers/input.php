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
     */
    public function simulate_basic($site, $year, $variety, $dateofsowing, $seeding) {
        $template_data = $this->run_templates_data_model->get_template($variety);

        $control_dat = $template_data['control_dat'];
        $experiment_data_dat = $template_data['experiment_data_dat'];
        $crop_data_dat = $template_data['crop_data_dat'];

        header("Content-Type: text/plain");
        echo $control_dat;

        echo $this->modify_control_dat($control_dat, $template_data['file_prefix']);
        echo $this->modify_experiment_data_dat($experiment_data_dat, $site, $year, $dateofsowing, $seeding);


        // print_r($experiment_data_dat);

        // print_r($crop_data_dat);
    }

    public function retrieve() {
        $this -> load -> database();
        $this -> load -> model('run_templates_data_model');

        $run_templates_data = $this -> db -> get('run_templates_data');

        foreach($run_templates_data -> result_object() as $result)
            print_r($result);
    }

    private function modify_control_dat($control_dat, $file_prefix) {
        $control_dat = preg_replace("/(FILEIT)(\\s*)(=)(\\s*)(\\\'.*?\\\')/", 'FILEIT = \''.$file_prefix.'.exp\'', $control_dat, 1);
        $control_dat = preg_replace("/(FILEI1)(\\s*)(=)(\\s*)(\\\'.*?\\\')/", 'FILEI1 = \''.$file_prefix.'.crp\'', $control_dat, 1);

        return $control_dat;
    }

    private function modify_experiment_data_dat($experiment_data_dat, $site, $year, $dateofsowing, $seeding) {
        //$experiment_data_dat = preg_replace("/(IYEAR)(\\s*)(=)(\\s+)((?:(?:[1]{1}\\d{1}\\d{1}\\d{1})|(?:[2]{1}\\d{3})))(?![\\d])/", 'IYEAR = '. $year, $experiment_data_dat);
        //$experiment_data_dat = preg_replace("/(EMYR)(\\s*)(=)(\\s+)((?:(?:[1]{1}\\d{1}\\d{1}\\d{1})|(?:[2]{1}\\d{3})))(?![\\d])/", 'EMYR = '. $year, $experiment_data_dat);




        return $experiment_data_dat;
    }
}