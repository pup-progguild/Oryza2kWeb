<?php
 /**
 * Run_templates_data_model
 *
 * @package	    Run templates data model
 * @subpackage	TODO
 * @category	Category
 * @author	    hoshi~
 * @link	    https://github.com/awkwardusername
 * @date        8/28/13 | 8:28 PM
 */

class Run_templates_data_model extends CI_Model {
    var $RUN_TEMPLATES_DATA_TABLE = 'run_templates_data';

    var $label = '';
    var $description = '';
    var $control_dat = '';
    var $experiment_data_dat = '';
    var $crop_data_dat = '';
    var $reruns_dat = '';
    var $preset = '';

    public function __construct() {
        parent::__construct();

        $this->load->database();
    }

    public function add() {
        $this->input->post('label');
        $this->input->post('description');
        $this->input->post('control_dat');
        $this->input->post('experiment_data_dat');
        $this->input->post('crop_data_dat');
        $this->input->post('reruns_dat');
        $this->input->post('preset');

        $this->db->insert($this->RUN_TEMPLATES_DATA_TABLE, $this);
    }

    public function update() {
        $this->input->post('label');
        $this->input->post('description');
        $this->input->post('control_dat');
        $this->input->post('experiment_data_dat');
        $this->input->post('crop_data_dat');
        $this->input->post('reruns_dat');
        $this->input->post('preset');

        $this->db->update($this->RUN_TEMPLATES_DATA_TABLE, $this, array('id' => $this->input->post('id')));
    }

    public function set_as_preset() {
        $this->input->post('preset');

        $this->db->update($this->RUN_TEMPLATES_DATA_TABLE, $this, array('id' => $this->input->post('id')));
    }
}