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

        $this->_init();

        $data['years'] = $this->weather_data_model -> get_years();
        $data['sites'] = $this->weather_data_model -> get_countries();
        $data['weather_years'] = $this->weather_data_model->get_country_year_list();

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
}