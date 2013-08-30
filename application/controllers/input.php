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

        $this->load->view('templates/header', $data);
        $this->load->view('templates/content-start', $data);
        $this->load->view('pages/input', $data);
        $this->load->view('templates/content-end', $data);
        $this->load->view('templates/footer', $data);
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


    public function retrieve($id = false, $file) {
        $this -> load -> database();
        $this -> load -> model('Run_templates_data_model', '', true);

        // TODO read models directly, not fetch directly from database
        $run_templates_data = $this -> db -> get('run_templates_data');

        $result_all = array();
        foreach($run_templates_data -> result_array() as $result) {
            if(!is_integer($id) && $result['id'] == $id) {
                echo $result[$file];
                return $result[$file];
            }
            else
                $result_all[] = $result;
        }

        if(count($result_all) > 1)
            return $result_all;
        return false;
    }
}