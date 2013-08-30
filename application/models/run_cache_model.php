<?php
 /**
 * Run_cache_model
 *
 * @package	    Run cache model
 * @subpackage	TODO
 * @category	Model
 * @author	    hoshi~
 * @link	    https://github.com/awkwardusername
 * @date        8/28/13 | 2:17 PM
 */

class Run_cache_model extends CI_Model {
    var $RUN_CACHE_TABLE = 'run_cache';

    var $op_dat = '';
    var $res_dat = '';
    var $hash_ident = '';
    var $run_templates_data_model_refFK = '';
    var $weather_data_refFK = '';

    public function __construct() {
        parent::__construct();

        $this->load->database();
    }

    public function add() {
        $this->input->post('op_dat');
        $this->input->post('res_dat');

        $this->db->insert($this->RUN_CACHE_TABLE, $this);
    }
}