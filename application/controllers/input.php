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
    private function _init() {
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
        $this->_init();
        $template = $this->run_templates_data_model -> get_template();
        header("Content-Type: text/plain");
        echo $template[$variety][$file];
    }

    public function simulate($site, $year, $variety, $dateofsowing, $seeding) {
        $data = array(
            'site' => $site,
            'year' => $year,
            'variety' => $variety,
            'dateofsowing' => $dateofsowing,
            'seeding' => $seeding
        );
    }

    public function retrieve() {
        $this -> load -> database();
        $this -> load -> model('Run_templates_data_model', '', true);

        $run_templates_data = $this -> db -> get('run_templates_data');

        foreach($run_templates_data -> result_object() as $result)
            print_r($result);
    }
}