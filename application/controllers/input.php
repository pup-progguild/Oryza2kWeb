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
        $this->load->helper('url');


    }

    public function index() {
        $data['title'] = 'Simulation';
        $data['year_start'] = 1991;
        $data['year_end'] = 1993;

        $this->_init();

        $data['template'] = $this->Run_templates_data_model->get_template();

        $this->load->view('templates/header', $data);
        $this->load->view('templates/content-start', $data);
        $this->load->view('pages/input', $data);
        $this->load->view('templates/content-end', $data);
        $this->load->view('templates/footer', $data);

        $this->load->model('Run_templates_data_model');



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