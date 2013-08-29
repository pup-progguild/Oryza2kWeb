<?php
 /**
 * Run_stream_model
 *
 * @package	    Run Stream Model
 * @subpackage	TODO
 * @category	Model
 * @author	    hoshi~
 * @link	    https://github.com/awkwardusername
 * @date        8/28/13 | 8:20 PM
 */

class Run_stream_model extends CI_Model {
    var $RUN_STREAM_TABLE = 'run_stream';

    var $run_cache_refFK = '';
    var $users_refFK = '';

    public function __construct() {
        parent::__construct();

        $this->load->database();
    }
}