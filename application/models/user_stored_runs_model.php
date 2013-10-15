<?php
 /**
 * User_stored_runs_model
 *
 * @package	    User stored runs model
 * @subpackage	TODO
 * @category	Model
 * @author	    hoshi~
 * @link	    https://github.com/awkwardusername
 * @date        8/28/13 | 8:15 PM
 */

class User_stored_runs_model extends CI_Model {
    var $USER_STORED_RUNS_TABLE = 'user_stored_runs';

    var $users_refFK = '';
    var $run_cache_refFK = '';

    public function __construct() {
        parent::__construct();

        $this->load->database();
    }


}